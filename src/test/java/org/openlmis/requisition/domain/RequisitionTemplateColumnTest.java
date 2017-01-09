package org.openlmis.requisition.domain;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.exception.ValidationMessageException;

public class RequisitionTemplateColumnTest {
  private RequisitionTemplateColumn requisitionTemplateColumn;

  @Before
  public void setUp() {
    requisitionTemplateColumn = new RequisitionTemplateColumn(null);
    requisitionTemplateColumn.setName("Test Column");
  }

  @Test
  public void testShouldChangeLabelOnlyIfValid() {
    requisitionTemplateColumn.setLabel("ValidName");
    Assert.assertEquals("ValidName", requisitionTemplateColumn.getLabel());
    requisitionTemplateColumn.setLabel("New valid name with numbers 123 and spaces ");
    Assert.assertEquals("New valid name with numbers 123 and spaces ",
        requisitionTemplateColumn.getLabel());
  }

  @Test(expected = ValidationMessageException.class)
  public void testShouldNotChangeLabelIfLabelNameIsInvalid() {
    requisitionTemplateColumn.setLabel("New not valid name with wrong signs: !@#$%^&*()");
  }

  @Test(expected = ValidationMessageException.class)
  public void testShouldNotChangeLabelIfLabelNameHasSpecialCharacters() {
    requisitionTemplateColumn.setLabel(")(*&^%$#@!");
  }

  @Test(expected = ValidationMessageException.class)
  public void testShouldNotChangeLabelIfLabelNameIsEmpty() {
    requisitionTemplateColumn.setLabel("");
  }

  @Test(expected = ValidationMessageException.class)
  public void testShouldNotChangeLabelIfLabelNameHasOnlyWhiteSpace() {
    requisitionTemplateColumn.setLabel(" ");
  }
}
