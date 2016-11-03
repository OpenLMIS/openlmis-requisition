package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Test;

import java.util.UUID;

public class RequisitionLineItemTest {

  @Test
  public void shouldOnlyUpdateApprovedFieldsWhenRequisitionStatusIsAuthorized() {
    Requisition requisition = mockReq(RequisitionStatus.AUTHORIZED);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setApprovedQuantity(1);
    requisitionLineItem.setRemarks("Remarks");
    requisitionLineItem.setStockOnHand(5);

    RequisitionLineItem updatedItem = new RequisitionLineItem();
    updatedItem.setRequisition(requisition);
    updatedItem.updateFrom(requisitionLineItem);

    assertEquals(1, updatedItem.getApprovedQuantity().intValue());
    assertEquals("Remarks", updatedItem.getRemarks());
    assertNull(updatedItem.getStockOnHand());
  }

  @Test
  public void shouldUpdateSubmissionFields() {
    Requisition requisition = mockReq(RequisitionStatus.INITIATED);
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);

    item.setStockOnHand(1);
    item.setTotalConsumedQuantity(2);
    item.setBeginningBalance(3);
    item.setTotalReceivedQuantity(4);
    item.setRequestedQuantity(5);
    item.setTotalStockoutDays(6);
    item.setTotal(7);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setStockOnHand(11);
    updateItem.setTotalConsumedQuantity(22);
    updateItem.setBeginningBalance(33);
    updateItem.setTotalReceivedQuantity(44);
    updateItem.setRequestedQuantity(55);
    updateItem.setTotalStockoutDays(66);
    updateItem.setTotal(77);

    item.updateFrom(updateItem);

    assertThat(item.getStockOnHand(), is(11));
    assertThat(item.getTotalConsumedQuantity(), is(22));
    assertThat(item.getBeginningBalance(), is(33));
    assertThat(item.getTotalReceivedQuantity(), is(44));
    assertThat(item.getRequestedQuantity(), is(55));
    assertThat(item.getTotalStockoutDays(), is(66));
    assertThat(item.getTotal(), is(77));

  }

  @Test
  public void shouldNotUpdateProduct() {
    final UUID product1 = UUID.randomUUID();
    final UUID product2 = UUID.randomUUID();

    Requisition requisition = mockReq(RequisitionStatus.INITIATED);
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    item.setOrderableProductId(product1);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setRequisition(requisition);
    updateItem.setOrderableProductId(product2);

    item.updateFrom(updateItem);

    assertThat(item.getOrderableProductId(), is(product1));
  }

  private Requisition mockReq(RequisitionStatus status) {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(status);
    return requisition;
  }
}
