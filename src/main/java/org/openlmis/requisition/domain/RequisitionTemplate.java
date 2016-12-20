package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Type;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.MapKeyColumn;
import javax.persistence.Table;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Entity
@Table(name = "requisition_templates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseTimestampedEntity {

  private static final String UUID = "pg-uuid";

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID programId;

  @ElementCollection(fetch = FetchType.EAGER)
  @MapKeyColumn(name = "key")
  @Column(name = "value")
  @CollectionTable(name = "columns_maps")
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
  public boolean isColumnDisplayed(String name) throws RequisitionTemplateColumnException {
    RequisitionTemplateColumn column = findColumn(name);

    return column.getIsDisplayed();
  }

  /**
   * Checks if column with given name is calculated.
   *
   * @param name name of requisition column.
   * @return return true if column is calculated
   */
  public boolean isColumnCalculated(String name) throws RequisitionTemplateColumnException {
    RequisitionTemplateColumn column = findColumn(name);

    return SourceType.CALCULATED.equals(column.getSource());
  }

  /**
   * Allows changing the display order of columns.
   *
   * @param key Key to column which needs a new display order.
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
   *
   * @param key Key to column which needs a new display property.
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
   *
   * @param key Key to column which needs a new name.
   * @param name New name for label.
   */
  public void changeColumnLabel(String key, String name)
      throws RequisitionTemplateColumnException {
    RequisitionTemplateColumn column = columnsMap.get(key);
    column.setLabel(name);
  }

  /**
   *
   * @param key Key to column which needs a new name.
   * @param source New source for column.
   */
  public void changeColumnSource(String key, SourceType source) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    column.setSource(source);
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
   * Finds a column by column name.
   *
   * @param name name of requisition column.
   * @return {@link RequisitionTemplateColumn} if found column with the given name.
   */
  private RequisitionTemplateColumn findColumn(String name)
      throws RequisitionTemplateColumnException {

    if (null == columnsMap) {
      throw new RequisitionTemplateColumnException("Column with name: " + name
          + " is not present in template");
    }

    RequisitionTemplateColumn column = columnsMap.get(name);

    if (column == null) {
      throw new RequisitionTemplateColumnException("Column with name: " + name
          + " is not present in template");
    }

    return column;
  }
}
