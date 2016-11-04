package org.openlmis.requisition.domain;

import com.google.common.collect.Lists;
import org.junit.Test;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;

import java.util.UUID;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

public class LineItemFieldsCalculatorTest {

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
    requisitionLineItem.setTotalLossesAndAdjustments(
        LineItemFieldsCalculator.calculateTotalLossesAndAdjustments(
            requisitionLineItem, Lists.newArrayList(reason1, reason2, reason3, reason4)));

    // then
    assertThat(requisitionLineItem.getTotalLossesAndAdjustments(), is(6));
  }

  @Test
  public void shouldCalculateBeginningBalanceBasedOnPrevious() throws Exception {
    RequisitionLineItem previous = new RequisitionLineItem();
    previous.setStockOnHand(3789);
    previous.setApprovedQuantity(714);

    assertThat(LineItemFieldsCalculator.calculateBeginningBalance(previous), is(4503));
  }

  @Test
  public void shouldSetZeroToBeginningBalanceIfPreviousNotExist() throws Exception {
    assertThat(LineItemFieldsCalculator.calculateBeginningBalance(null), is(0));
  }

  @Test
  public void shouldSetZeroToBeginningBalanceIfPreviousNotHasData() throws Exception {
    assertThat(LineItemFieldsCalculator.calculateBeginningBalance(new RequisitionLineItem()),
        is(0));
  }

  @Test
  public void shouldCalculateStockOnHand() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalLossesAndAdjustments(-100);
    requisitionLineItem.setTotalConsumedQuantity(200);
    requisitionLineItem.setTotalReceivedQuantity(500);
    requisitionLineItem.setBeginningBalance(1000);

    assertEquals(1200, LineItemFieldsCalculator.calculateStockOnHand(requisitionLineItem));
  }

  @Test
  public void shouldCalculateStockOnHandIfNull() throws Exception {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalLossesAndAdjustments(null);
    requisitionLineItem.setTotalConsumedQuantity(200);
    requisitionLineItem.setTotalReceivedQuantity(500);
    requisitionLineItem.setBeginningBalance(1000);

    assertEquals(1300, LineItemFieldsCalculator.calculateStockOnHand(requisitionLineItem));
  }

  @Test
  public void shouldCalculateTotal() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalReceivedQuantity(500);
    requisitionLineItem.setBeginningBalance(1000);

    assertEquals(1500, LineItemFieldsCalculator.calculateTotal(requisitionLineItem));
  }

  @Test
  public void shouldCalculateTotalConsumedQuantity() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalLossesAndAdjustments(-800);
    requisitionLineItem.setStockOnHand(300);
    requisitionLineItem.setTotalReceivedQuantity(500);
    requisitionLineItem.setBeginningBalance(1000);

    assertEquals(400, LineItemFieldsCalculator.calculateTotalConsumedQuantity(requisitionLineItem));
  }
}
