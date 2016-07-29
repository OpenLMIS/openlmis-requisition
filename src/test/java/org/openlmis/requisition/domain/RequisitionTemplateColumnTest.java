package org.openlmis.requisition.domain;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class RequisitionTemplateColumnTest {
  RequisitionTemplateColumn requisitionTemplateColumn;

  @Before
  public void setUp() {
    requisitionTemplateColumn = new RequisitionTemplateColumn();
    requisitionTemplateColumn.setName("Test Column");
    requisitionTemplateColumn.setLabel("Label");
  }

  @Test
  public void changeLabelTest() {
    boolean result = requisitionTemplateColumn.changeLabel("ValidName");
    Assert.assertTrue(result);
    result =
        requisitionTemplateColumn.changeLabel("New valid name with numbers 123 and spaces ");
    Assert.assertTrue(result);
    result =
        requisitionTemplateColumn.changeLabel("New not valid name with wrong signs: !@#$%^&*()");
    Assert.assertFalse(result);
    result = requisitionTemplateColumn.changeLabel("!@#$%^&*()");
    Assert.assertFalse(result);
    result = requisitionTemplateColumn.changeLabel("");
    Assert.assertFalse(result);
    result = requisitionTemplateColumn.changeLabel(" ");
    Assert.assertFalse(result);
  }
}
