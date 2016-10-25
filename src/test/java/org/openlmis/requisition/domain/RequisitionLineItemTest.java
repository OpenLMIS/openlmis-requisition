package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;

import org.junit.Test;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;

import java.util.UUID;

public class RequisitionLineItemTest {

  @Test
  public void shouldCalculateTotalLossesAndAdjustments() throws Exception {
    // given
    UUID id1 = UUID.randomUUID();
    UUID id2 = UUID.randomUUID();
    UUID id3 = UUID.randomUUID();
    UUID id4 = UUID.randomUUID();

    StockAdjustmentReasonDto reason1 = mock(StockAdjustmentReasonDto.class);
    StockAdjustmentReasonDto reason2 = mock(StockAdjustmentReasonDto.class);
    StockAdjustmentReasonDto reason3 = mock(StockAdjustmentReasonDto.class);
    StockAdjustmentReasonDto reason4 = mock(StockAdjustmentReasonDto.class);

    StockAdjustment adjustment1 = mock(StockAdjustment.class);
    StockAdjustment adjustment2 = mock(StockAdjustment.class);
    StockAdjustment adjustment3 = mock(StockAdjustment.class);
    StockAdjustment adjustment4 = mock(StockAdjustment.class);

    // when
    doReturn(true).when(reason1).getAdditive();
    doReturn(id1).when(reason1).getId();
    doReturn(true).when(reason2).getAdditive();
    doReturn(id2).when(reason2).getId();
    doReturn(false).when(reason3).getAdditive();
    doReturn(id3).when(reason3).getId();
    doReturn(false).when(reason4).getAdditive();
    doReturn(id4).when(reason4).getId();

    doReturn(5).when(adjustment1).getQuantity();
    doReturn(id1).when(adjustment1).getReasonId();
    doReturn(10).when(adjustment2).getQuantity();
    doReturn(id2).when(adjustment2).getReasonId();
    doReturn(2).when(adjustment3).getQuantity();
    doReturn(id3).when(adjustment3).getReasonId();
    doReturn(7).when(adjustment4).getQuantity();
    doReturn(id4).when(adjustment4).getReasonId();

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setStockAdjustments(Lists.newArrayList(adjustment1,adjustment2,
        adjustment3,adjustment4));
    requisitionLineItem.calculateTotalLossesAndAdjustments(Lists.newArrayList(reason1,
        reason2,reason3,reason4));

    // then
    assertThat(requisitionLineItem.getTotalLossesAndAdjustments(), is(6));
  }

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

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setStockOnHand(11);
    updateItem.setTotalConsumedQuantity(22);
    updateItem.setBeginningBalance(33);
    updateItem.setTotalReceivedQuantity(44);
    updateItem.setRequestedQuantity(55);

    item.updateFrom(updateItem);

    assertThat(item.getStockOnHand(), is(11));
    assertThat(item.getTotalConsumedQuantity(), is(22));
    assertThat(item.getBeginningBalance(), is(33));
    assertThat(item.getTotalReceivedQuantity(), is(44));
    assertThat(item.getRequestedQuantity(), is(55));
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
