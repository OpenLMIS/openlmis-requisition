package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.MapKeyColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "requisition_templates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "programId", nullable = false, unique = true)
  @Getter
  @Setter
  private Program program;

  @ElementCollection(fetch = FetchType.EAGER)
  @MapKeyColumn(name = "key")
  @Column(name = "value")
  @Getter
  @Setter
  private Map<String,RequisitionTemplateColumn> columnsMap = new HashMap<>();

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
   *
   * @param key Key to column which needs a new name.
   * @param canBeChangedByUser Is it allowed to change column cells value by user input.
   */
  public void changeCanBeChangedByUserSetting(String key, Boolean canBeChangedByUser) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    column.setCanBeChangedByUser(canBeChangedByUser);
    columnsMap.put(key, column);
  }
}
