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
  public void testShouldChangeLabelOnlyIfValid() {
    boolean result = requisitionTemplateColumn.setLabel("ValidName");
    Assert.assertTrue(result);
    result =
        requisitionTemplateColumn.setLabel("New valid name with numbers 123 and spaces ");
    Assert.assertTrue(result);
  }

  @Test
  public void testShouldNotChangeLabelIfInvalid() {
    boolean result =
        requisitionTemplateColumn.setLabel("New not valid name with wrong signs: !@#$%^&*()");
    Assert.assertFalse(result);
    result = requisitionTemplateColumn.setLabel("!@#$%^&*()");
    Assert.assertFalse(result);
    result = requisitionTemplateColumn.setLabel("");
    Assert.assertFalse(result);
    result = requisitionTemplateColumn.setLabel(" ");
    Assert.assertFalse(result);
  }
}
