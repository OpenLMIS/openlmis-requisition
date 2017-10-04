/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.openlmis.requisition.domain.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class LineItemFieldsCalculatorTest {

  private static final int STOCK_ON_HAND = 3789;

  @Test
  public void shouldCalculateTotalLossesAndAdjustments() throws Exception {
    // given
    UUID id1 = UUID.randomUUID();
    UUID id2 = UUID.randomUUID();
    UUID id3 = UUID.randomUUID();
    UUID id4 = UUID.randomUUID();

    StockAdjustmentReason reason1 = mock(StockAdjustmentReason.class);
    StockAdjustmentReason reason2 = mock(StockAdjustmentReason.class);
    StockAdjustmentReason reason3 = mock(StockAdjustmentReason.class);
    StockAdjustmentReason reason4 = mock(StockAdjustmentReason.class);

    StockAdjustment adjustment1 = mock(StockAdjustment.class);
    StockAdjustment adjustment2 = mock(StockAdjustment.class);
    StockAdjustment adjustment3 = mock(StockAdjustment.class);
    StockAdjustment adjustment4 = mock(StockAdjustment.class);

    // when
    doReturn(true).when(reason1).isCreditReasonType();
    doReturn(id1).when(reason1).getReasonId();
    doReturn(true).when(reason2).isCreditReasonType();
    doReturn(id2).when(reason2).getReasonId();
    doReturn(false).when(reason3).isCreditReasonType();
    doReturn(id3).when(reason3).getReasonId();
    doReturn(false).when(reason4).isCreditReasonType();
    doReturn(id4).when(reason4).getReasonId();

    doReturn(5).when(adjustment1).getQuantity();
    doReturn(id1).when(adjustment1).getReasonId();
    doReturn(10).when(adjustment2).getQuantity();
    doReturn(id2).when(adjustment2).getReasonId();
    doReturn(2).when(adjustment3).getQuantity();
    doReturn(id3).when(adjustment3).getReasonId();
    doReturn(7).when(adjustment4).getQuantity();
    doReturn(id4).when(adjustment4).getReasonId();

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setStockAdjustments(Lists.newArrayList(adjustment1, adjustment2,
        adjustment3, adjustment4));
    requisitionLineItem.setTotalLossesAndAdjustments(
        LineItemFieldsCalculator.calculateTotalLossesAndAdjustments(
            requisitionLineItem, Lists.newArrayList(reason1, reason2, reason3, reason4)));

    // then
    assertThat(requisitionLineItem.getTotalLossesAndAdjustments(), is(6));
  }

  @Test
  public void shouldCalculateBeginningBalanceBasedOnPrevious() throws Exception {
    RequisitionLineItem previous = new RequisitionLineItem();
    previous.setStockOnHand(STOCK_ON_HAND);

    assertThat(LineItemFieldsCalculator.calculateBeginningBalance(previous), is(STOCK_ON_HAND));
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

  @Test
  public void shouldCalculateTotalCost() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setPricePerPack(Money.of(CurrencyUnit.USD, 3.25));
    requisitionLineItem.setPacksToShip(40L);

    Money totalCost =
        LineItemFieldsCalculator.calculateTotalCost(requisitionLineItem, CurrencyUnit.USD);

    assertEquals(Money.of(CurrencyUnit.USD, 130), totalCost);
  }

  @Test
  public void shouldCalculateTotalCostAsZeroIfValuesAreMissing() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setPricePerPack(Money.of(CurrencyUnit.USD, 3.25));
    requisitionLineItem.setPacksToShip(null);

    Money totalCost =
        LineItemFieldsCalculator.calculateTotalCost(requisitionLineItem, CurrencyUnit.USD);
    assertEquals(BigDecimal.ZERO.setScale(2, RoundingMode.UNNECESSARY), totalCost.getAmount());

    requisitionLineItem.setPricePerPack(null);
    totalCost = LineItemFieldsCalculator.calculateTotalCost(requisitionLineItem, CurrencyUnit.USD);
    assertEquals(BigDecimal.ZERO.setScale(2, RoundingMode.UNNECESSARY), totalCost.getAmount());

    requisitionLineItem.setPacksToShip(20L);
    totalCost = LineItemFieldsCalculator.calculateTotalCost(requisitionLineItem, CurrencyUnit.USD);
    assertEquals(BigDecimal.ZERO.setScale(2, RoundingMode.UNNECESSARY), totalCost.getAmount());
  }

  @Test
  public void shouldCalculateAdjustedConsumption() throws Exception {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalStockoutDays(5);
    requisitionLineItem.setTotalConsumedQuantity(20);

    assertEquals(22, LineItemFieldsCalculator.calculateAdjustedConsumption(requisitionLineItem, 3));
  }

  @Test
  public void shouldCalculateAdjustedConsumptionWhenNonStockoutDaysIsZero() throws Exception {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalStockoutDays(90);
    requisitionLineItem.setTotalConsumedQuantity(20);

    assertEquals(20, LineItemFieldsCalculator.calculateAdjustedConsumption(requisitionLineItem, 3));
  }

  @Test
  public void shouldCalculateAdjustedConsumptionWhenNonStockoutDaysIsLessThanZero() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setTotalStockoutDays(92);
    requisitionLineItem.setTotalConsumedQuantity(20);

    assertEquals(20, LineItemFieldsCalculator.calculateAdjustedConsumption(requisitionLineItem, 3));
  }

  @Test
  public void shouldCalculateAverageConsumption() throws Exception {
    int averageConsumption =
        LineItemFieldsCalculator.calculateAverageConsumption(Arrays.asList(5, 10, 15));

    assertEquals(10, averageConsumption);
  }

  @Test
  public void shouldReturnAdjustedConsumptionWhenNoPreviousPeriods() throws Exception {
    int averageConsumption = LineItemFieldsCalculator
        .calculateAverageConsumption(Collections.singletonList(5));

    assertEquals(5, averageConsumption);
  }

  @Test
  public void shouldCalculateAverageConsumptionWhenOnePreviousPeriod() throws Exception {
    int averageConsumption =
        LineItemFieldsCalculator.calculateAverageConsumption(Arrays.asList(5, 10));

    assertEquals(8, averageConsumption);
  }

  @Test
  public void shouldCalculateMaximumStockQuantityForDefaultOption() throws Exception {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();
    column.setOption(new AvailableRequisitionColumnOption(null, "default", "Default"));

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(ImmutableMap.of(MAXIMUM_STOCK_QUANTITY, column));

    RequisitionLineItem item = new RequisitionLineItem();
    item.setMaxPeriodsOfStock(BigDecimal.valueOf(7.25));
    item.setAverageConsumption(2);

    assertThat(
        LineItemFieldsCalculator.calculateMaximumStockQuantity(item, template),
        is(equalTo(15))
    );
  }

  @Test
  public void shouldCalculateMaximumStockQuantityWhenOptionIsNotSelected() throws Exception {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(ImmutableMap.of(MAXIMUM_STOCK_QUANTITY, column));

    RequisitionLineItem item = new RequisitionLineItem();
    item.setMaxPeriodsOfStock(BigDecimal.valueOf(7.25));
    item.setAverageConsumption(2);

    assertThat(
        LineItemFieldsCalculator.calculateMaximumStockQuantity(item, template),
        is(equalTo(15))
    );
  }

  @Test
  public void shouldCalculateCalculatedOrderQuantity() throws Exception {

    RequisitionLineItem item = new RequisitionLineItem();
    item.setStockOnHand(5);
    item.setMaximumStockQuantity(10);

    assertThat(
        LineItemFieldsCalculator.calculateCalculatedOrderQuantity(item, new RequisitionTemplate()),
        is(equalTo(5))
    );

  }

  @Test
  public void shouldCalculateCalculatedOrderQuantityIfMaximumStockQuantityIsNotSet() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();
    column.setOption(new AvailableRequisitionColumnOption(null, "default", "Default"));

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(ImmutableMap.of(MAXIMUM_STOCK_QUANTITY, column));

    RequisitionLineItem item = new RequisitionLineItem();
    item.setMaxPeriodsOfStock(BigDecimal.valueOf(7));
    item.setAverageConsumption(2);
    item.setStockOnHand(4);

    assertThat(
        LineItemFieldsCalculator.calculateCalculatedOrderQuantity(item, template),
        is(equalTo(10))
    );

  }

  @Test
  public void shouldCalculateCalculatedOrderQuantityIfStockOnHandIsNotSet() {

    RequisitionLineItem item = new RequisitionLineItem();
    item.setStockOnHand((Integer) null);
    item.setBeginningBalance(5);
    item.setTotalReceivedQuantity(0);
    item.setTotalLossesAndAdjustments(0);
    item.setTotalConsumedQuantity(0);
    item.setMaximumStockQuantity(10);

    assertThat(
        LineItemFieldsCalculator.calculateCalculatedOrderQuantity(item, new RequisitionTemplate()),
        is(equalTo(5))
    );

  }

}
