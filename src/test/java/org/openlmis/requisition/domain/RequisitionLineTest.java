package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class RequisitionLineTest {

  @Test
  public void shouldCalculateStockOnHand() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setTotalLossesAndAdjustments(-100);
    requisitionLine.setTotalConsumedQuantity(200);
    requisitionLine.setTotalReceivedQuantity(500);
    requisitionLine.setBeginningBalance(1000);

    requisitionLine.calculateStockOnHand();

    assertEquals(1200, requisitionLine.getStockOnHand().intValue());
  }
}
