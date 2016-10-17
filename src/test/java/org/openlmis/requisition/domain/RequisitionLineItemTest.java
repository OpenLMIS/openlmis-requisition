package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;

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
}
