package org.openlmis.requisition.domain;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Type;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.MapKeyColumn;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import static org.apache.commons.lang.StringUtils.equalsIgnoreCase;

@Entity
@Table(name = "requisition_templates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseEntity {

  private static final String UUID = "pg-uuid";

  @Column(unique = true)
  @Getter
  @Setter
  @Type(type = UUID)
  private UUID program;

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
    if (column.getCanChangeOrder()) {
      column.setDisplayOrder(newDisplayOrder);
      columnsMap.put(key, column);
    }
  }

  private void moveDownAllColumnsBelowIndex(int beginIndex) {
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columnsMap.entrySet()) {
      RequisitionTemplateColumn tempColumn = entry.getValue();
      if (tempColumn.getDisplayOrder() >= beginIndex) {
        tempColumn.setDisplayOrder(tempColumn.getDisplayOrder() + 1);
        columnsMap.put(entry.getKey(), tempColumn);
      }
    }
  }

  private void moveUpAllColumnsBetweenIndexes(int beginIndex, int endIndex) {
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columnsMap.entrySet()) {
      RequisitionTemplateColumn tempColumn = entry.getValue();
      if (tempColumn.getDisplayOrder() <= beginIndex && tempColumn.getDisplayOrder() > endIndex) {
        tempColumn.setDisplayOrder(tempColumn.getDisplayOrder() - 1);
        columnsMap.put(entry.getKey(), tempColumn);
      }
    }
  }

  private void moveDownAllColumnsBetweenIndexes(int beginIndex, int endIndex) {
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columnsMap.entrySet()) {
      RequisitionTemplateColumn tempColumn = entry.getValue();
      if (tempColumn.getDisplayOrder() >= beginIndex && tempColumn.getDisplayOrder() < endIndex) {
        tempColumn.setDisplayOrder(tempColumn.getDisplayOrder() + 1);
        columnsMap.put(entry.getKey(), tempColumn);
      }
    }
  }

  /**
   *
   * @param key Key to column which needs a new display property.
   * @param display Should column be displayed.
   */
  public void changeColumnDisplay(String key, boolean display) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    if (!column.getIsDisplayRequired()) {
      if (display && key.equals("productCode")) {
        column.setDisplayOrder(1);
      }
      column.setIsDisplayed(display);
      columnsMap.put(key, column);
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
    columnsMap.put(key, column);
  }

  /**
   *
   * @param key Key to column which needs a new name.
   * @param source New source for column.
   */
  public void changeColumnSource(String key, SourceType source) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    column.setSource(source);
    columnsMap.put(key, column);
  }

  /**
   * Copy values of attributes into new or updated RequisitionTemplate.
   *
   * @param requisitionTemplate RequisitionTemplate with new values.
   */
  public void updateFrom(RequisitionTemplate requisitionTemplate) {
    this.program = requisitionTemplate.getProgram();
    this.columnsMap = requisitionTemplate.getColumnsMap();
  }

  /**
   * Finds a column by indicator of column definition.
   *
   * @param indicator single letter of available requisition column.
   * @return {@link RequisitionTemplateColumn} if found column with definition with the given
   *           indicator; otherwise {@code null}.
   */
  public RequisitionTemplateColumn findColumn(String indicator) {
    if (null == columnsMap || columnsMap.isEmpty()) {
      return null;
    }

    return columnsMap.values().stream()
        .filter(c -> hasColumn(c, indicator))
        .findFirst().orElse(null);
  }

  private boolean hasColumn(RequisitionTemplateColumn column, String indicator) {
    if (null == column) {
      return false;
    }

    AvailableRequisitionColumn definition = column.getColumnDefinition();

    if (null == definition) {
      return false;
    }

    return equalsIgnoreCase(definition.getIndicator(), indicator);
  }
}
