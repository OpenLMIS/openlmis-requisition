package org.openlmis.requisition.domain;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RequisitionTemplateTest {

  private RequisitionTemplate requisitionTemplate;

  private static final String[] COLUMN_NAMES = {"column1", "column2", "column3", "column4"};

  @Before
  public void setUp() {
    requisitionTemplate = new RequisitionTemplate();
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setCanChangeOrder(true);
    RequisitionTemplateColumn column1 = new RequisitionTemplateColumn(columnDefinition);
    column1.setName(COLUMN_NAMES[0]);
    column1.setDisplayOrder(1);
    RequisitionTemplateColumn column2 = new RequisitionTemplateColumn(columnDefinition);
    column2.setName(COLUMN_NAMES[1]);
    column2.setDisplayOrder(2);
    RequisitionTemplateColumn column3 = new RequisitionTemplateColumn(columnDefinition);
    column3.setName(COLUMN_NAMES[2]);
    column3.setDisplayOrder(3);
    RequisitionTemplateColumn column4 = new RequisitionTemplateColumn(columnDefinition);
    column4.setName(COLUMN_NAMES[3]);
    column4.setDisplayOrder(4);
    Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<String,
        RequisitionTemplateColumn>();
    columnsMap.put(column1.getName(), column1);
    columnsMap.put(column2.getName(), column2);
    columnsMap.put(column3.getName(), column3);
    columnsMap.put(column4.getName(), column4);
    requisitionTemplate.setColumnsMap(columnsMap);
  }

  @Test
  public void testChangeColumnDisplayOrderToLower() {
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_NAMES[2], 1);
    Map<String, RequisitionTemplateColumn> mapAfterChange = requisitionTemplate.getColumnsMap();
    Assert.assertEquals(1, mapAfterChange.get(COLUMN_NAMES[2]).getDisplayOrder());
    Assert.assertEquals(2, mapAfterChange.get(COLUMN_NAMES[0]).getDisplayOrder());
    Assert.assertEquals(3, mapAfterChange.get(COLUMN_NAMES[1]).getDisplayOrder());
    Assert.assertEquals(4, mapAfterChange.get(COLUMN_NAMES[3]).getDisplayOrder());
  }

  @Test
  public void testChangeColumnDisplayOrderToHigher() {
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_NAMES[0], 3);
    Map<String, RequisitionTemplateColumn> mapAfterChange = requisitionTemplate.getColumnsMap();
    Assert.assertEquals(1, mapAfterChange.get(COLUMN_NAMES[1]).getDisplayOrder());
    Assert.assertEquals(2, mapAfterChange.get(COLUMN_NAMES[2]).getDisplayOrder());
    Assert.assertEquals(3, mapAfterChange.get(COLUMN_NAMES[0]).getDisplayOrder());
    Assert.assertEquals(4, mapAfterChange.get(COLUMN_NAMES[3]).getDisplayOrder());
  }

  @Test
  public void testChangeColumnDisplayOrderToTheSame() {
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_NAMES[1], 2);
    Map<String, RequisitionTemplateColumn> mapAfterChange = requisitionTemplate.getColumnsMap();
    Assert.assertEquals(1, mapAfterChange.get(COLUMN_NAMES[0]).getDisplayOrder());
    Assert.assertEquals(2, mapAfterChange.get(COLUMN_NAMES[1]).getDisplayOrder());
    Assert.assertEquals(3, mapAfterChange.get(COLUMN_NAMES[2]).getDisplayOrder());
    Assert.assertEquals(4, mapAfterChange.get(COLUMN_NAMES[3]).getDisplayOrder());
  }

  @Test
  public void shouldCheckIfItHasColumnsDefined() {
    assertTrue(requisitionTemplate.hasColumnsDefined());
    assertFalse(new RequisitionTemplate(new HashMap<>()).hasColumnsDefined());
  }
}
