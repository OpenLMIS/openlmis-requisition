package org.openlmis.requisition.domain;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

public class RequisitionTemplateTest {

  RequisitionTemplate requisitionTemplate;

  private static final String[] COLUMN_NAMES = {"column1", "column2", "column3", "column4"};

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    requisitionTemplate = new RequisitionTemplate();
    RequisitionTemplateColumn column1 = new RequisitionTemplateColumn();
    column1.setName(COLUMN_NAMES[0]);
    column1.setDisplayOrder(1);
    column1.setCanChangeOrder(true);
    RequisitionTemplateColumn column2 = new RequisitionTemplateColumn();
    column2.setName(COLUMN_NAMES[1]);
    column2.setDisplayOrder(2);
    column2.setCanChangeOrder(true);
    RequisitionTemplateColumn column3 = new RequisitionTemplateColumn();
    column3.setName(COLUMN_NAMES[2]);
    column3.setDisplayOrder(3);
    column3.setCanChangeOrder(true);
    RequisitionTemplateColumn column4 = new RequisitionTemplateColumn();
    column4.setName(COLUMN_NAMES[3]);
    column4.setDisplayOrder(4);
    column4.setCanChangeOrder(true);
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
}
