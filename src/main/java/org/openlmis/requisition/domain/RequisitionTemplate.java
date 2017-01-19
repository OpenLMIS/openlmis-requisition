package org.openlmis.requisition.domain;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMNS_MAP_IS_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_NOT_IN_TEMPLATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.hibernate.annotations.Type;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.utils.Message;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.MapKeyColumn;
import javax.persistence.Table;

@SuppressWarnings("PMD.TooManyMethods")
@Entity
@Table(name = "requisition_templates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseTimestampedEntity {

  private static final String UUID = "pg-uuid";
  public static final String SOURCE = "Source ";
  public static final String OPTION = "Option ";
  public static final String WARNING_SUFFIX = " is not available for this column.";

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID programId;

  @Getter
  @Setter
  private Integer numberOfPeriodsToAverage;

  @ElementCollection(fetch = FetchType.EAGER)
  @MapKeyColumn(name = "key")
  @Column(name = "value")
  @CollectionTable(
      name = "columns_maps",
      joinColumns = @JoinColumn(name = "requisitionTemplateId"))
  @Getter
  @Setter
  private Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<>();

  /**
   * Allows creating requisition template with predefined columns.
   *
   * @param columns Columns to appear in requisition template.
   */
  public RequisitionTemplate(Map<String, RequisitionTemplateColumn> columns) {
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columns.entrySet()) {
      columnsMap.put(entry.getKey(), entry.getValue());
    }
  }

  /**
   * Checks if column with given name is displayed.
   *
   * @param name name of requisition column.
   * @return return true if column is displayed
   */
  public boolean isColumnDisplayed(String name) {
    RequisitionTemplateColumn column = findColumn(name);

    return column.getIsDisplayed();
  }

  /**
   * Checks if column with given name is calculated.
   *
   * @param name name of requisition column.
   * @return return true if column is calculated
   */
  public boolean isColumnCalculated(String name) {
    RequisitionTemplateColumn column = findColumn(name);

    return SourceType.CALCULATED.equals(column.getSource());
  }

  /**
   * Checks if column with given name is input by user.
   *
   * @param name name of requisition column.
   * @return return true if column is calculated
   */
  public boolean isColumnUserInput(String name) {
    RequisitionTemplateColumn column = findColumn(name);

    return SourceType.USER_INPUT.equals(column.getSource());
  }

  /**
   * Allows changing the display order of columns.
   *
   * @param key             Key to column which needs a new display order.
   * @param newDisplayOrder Number specifying new display order of extracted column.
   */
  public void changeColumnDisplayOrder(String key, int newDisplayOrder) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    Integer oldDisplayOrder = column.getDisplayOrder();
    if (oldDisplayOrder == null) {
      moveDownAllColumnsBelowIndex(newDisplayOrder);
    } else {
      if (newDisplayOrder > oldDisplayOrder) {
        moveUpAllColumnsBetweenIndexes(newDisplayOrder, oldDisplayOrder);
      } else {
        moveDownAllColumnsBetweenIndexes(newDisplayOrder, oldDisplayOrder);
      }
    }
    if (column.getColumnDefinition().getCanChangeOrder()) {
      column.setDisplayOrder(newDisplayOrder);
    }
  }

  /**
   * @param key     Key to column which needs a new display property.
   * @param display Should column be displayed.
   */
  public void changeColumnDisplay(String key, boolean display) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    if (!column.getColumnDefinition().getIsDisplayRequired()) {
      if (display && "productCode".equals(key)) {
        column.setDisplayOrder(1);
      }
      column.setIsDisplayed(display);
    }
  }

  /**
   * @param key  Key to column which needs a new name.
   * @param name New name for label.
   */
  public void changeColumnLabel(String key, String name) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    column.setLabel(name);
  }

  /**
   * Validate source of column and change it if it's available.
   *
   * @param key    Key to column which needs a new source.
   * @param source New source for column.
   */
  public void changeColumnSource(String key, SourceType source) {

    RequisitionTemplateColumn column = findColumn(key);

    if (column.getColumnDefinition().getSources() == null) {
      throw new ValidationMessageException(new Message(ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN,
          source.toString()));
    }

    if (!column.getColumnDefinition().getSources().contains(source)) {
      throw new ValidationMessageException(new Message(ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN,
          source.toString()));
    }
    column.setSource(source);
  }

  /**
   * Validate option of column and change it if it's available.
   *
   * @param key    Key to column which needs a new option.
   * @param option New option for column.
   */
  public void changeColumnOption(String key, AvailableRequisitionColumnOption option) {

    RequisitionTemplateColumn column = findColumn(key);

    if (column.getColumnDefinition().getOptions() == null) {
      throw new ValidationMessageException(new Message(ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN,
          option.getOptionName()));
    }

    if (!column.getColumnDefinition().getOptions().contains(option)) {
      throw new ValidationMessageException(new Message(ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN,
          option.getOptionName()));
    }
    column.setOption(option);
  }

  /**
   * Copy values of attributes into new or updated RequisitionTemplate.
   *
   * @param requisitionTemplate RequisitionTemplate with new values.
   */
  public void updateFrom(RequisitionTemplate requisitionTemplate) {
    this.programId = requisitionTemplate.getProgramId();
    this.columnsMap = requisitionTemplate.getColumnsMap();
  }

  public boolean hasColumnsDefined() {
    return columnsMap != null && !columnsMap.isEmpty();
  }

  /**
   * Checks if column with given name is defined in the template.
   *
   * @param columnName name of requisition column.
   * @return return true if column is defined in the template.
   */
  public boolean isColumnInTemplate(String columnName) {
    return getRequisitionTemplateColumn(columnName) != null;
  }

  private void moveDownAllColumnsBelowIndex(int beginIndex) {
    for (RequisitionTemplateColumn column : columnsMap.values()) {
      if (column.getDisplayOrder() >= beginIndex) {
        column.setDisplayOrder(column.getDisplayOrder() + 1);
      }
    }
  }

  private void moveUpAllColumnsBetweenIndexes(int beginIndex, int endIndex) {
    for (RequisitionTemplateColumn column : columnsMap.values()) {
      if (column.getDisplayOrder() <= beginIndex && column.getDisplayOrder() > endIndex) {
        column.setDisplayOrder(column.getDisplayOrder() - 1);
      }
    }
  }

  private void moveDownAllColumnsBetweenIndexes(int beginIndex, int endIndex) {
    for (RequisitionTemplateColumn column : columnsMap.values()) {
      if (column.getDisplayOrder() >= beginIndex && column.getDisplayOrder() < endIndex) {
        column.setDisplayOrder(column.getDisplayOrder() + 1);
      }
    }
  }

  /**
   * Finds a column by column name or throws exception.
   *
   * @param name name of requisition column.
   * @return {@link RequisitionTemplateColumn} if found column with the given name.
   */
  public RequisitionTemplateColumn findColumn(String name) {
    RequisitionTemplateColumn column = getRequisitionTemplateColumn(name);
    if (column == null) {
      throw new ValidationMessageException(new Message(ERROR_COLUMN_NOT_IN_TEMPLATE, name));
    }
    return column;
  }

  private RequisitionTemplateColumn getRequisitionTemplateColumn(String name) {
    if (columnsMap == null) {
      throw new ValidationMessageException(new Message(ERROR_COLUMNS_MAP_IS_NULL));
    }
    return columnsMap.get(name);
  }
}
