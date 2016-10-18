package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Test;

public class RequisitionLineItemTest {

  @Test
  public void shouldCalculateStockOnHand() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalLossesAndAdjustments(-100);
    requisitionLineItem.setTotalConsumedQuantity(200);
    requisitionLineItem.setTotalReceivedQuantity(500);
    requisitionLineItem.setBeginningBalance(1000);

    requisitionLineItem.calculateStockOnHand();

    assertEquals(1200, requisitionLineItem.getStockOnHand().intValue());
  }

  @Test
  public void shouldCalculateStockOnHandIfNull() throws Exception {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalLossesAndAdjustments(null);
    requisitionLineItem.setTotalConsumedQuantity(200);
    requisitionLineItem.setTotalReceivedQuantity(500);
    requisitionLineItem.setBeginningBalance(1000);

    requisitionLineItem.calculateStockOnHand();

    assertEquals(1300, requisitionLineItem.getStockOnHand().intValue());
  }

  @Test
  public void shouldOnlyUpdateApprovedFieldsWhenRequisitionStatusIsAuthorized() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setApprovedQuantity(1);
    requisitionLineItem.setRemarks("Remarks");
    requisitionLineItem.setStockOnHand(5);

    RequisitionLineItem updatedItem = new RequisitionLineItem();
    updatedItem.updateFrom(requisitionLineItem);

    assertEquals(1, updatedItem.getApprovedQuantity().intValue());
    assertEquals("Remarks", updatedItem.getRemarks());
    assertNull(updatedItem.getStockOnHand());
  }
}
