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

package org.openlmis.requisition.domain.requisition;

import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.stockmanagement.StockAdjustmentDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineItemTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  private Requisition initiatedRequisition;

  private UUID programId = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();
  private Money pricePerPack = Money.of(CurrencyUnit.USD, 5.79);
  private double maxPeriodsOfStock = 7.25;

  @Before
  public void setUp() {
    initiatedRequisition = mockReq(RequisitionStatus.INITIATED);
  }

  @Test
  public void shouldCreateNewRequisitionLineItem() {
    RequisitionLineItem item =
        new RequisitionLineItem(initiatedRequisition,
            createDefaultApprovedProduct(pricePerPack));

    checkResultsOfConstruction(item);
    assertEquals(pricePerPack, item.getPricePerPack());
  }

  @Test
  public void shouldCreateNewRequisitionLineItemWithDefaultPricePerPack() {
    RequisitionLineItem item =
        new RequisitionLineItem(initiatedRequisition,
            createDefaultApprovedProduct(null));

    checkResultsOfConstruction(item);
    assertEquals(Money.of(CurrencyUnit.USD, RequisitionLineItem.PRICE_PER_PACK_IF_NULL),
        item.getPricePerPack());
  }

  @Test
  public void shouldResetData() {
    RequisitionLineItem item = createDefaultRequisitionLineItem(
        createDefaultApprovedProduct(null), Collections.emptyList()
    );

    item.resetData();

    assertNull(item.getTotalReceivedQuantity());
    assertNull(item.getTotalLossesAndAdjustments());
    assertNull(item.getStockOnHand());
    assertNull(item.getRequestedQuantityExplanation());
    assertNull(item.getRemarks());
    assertNull(item.getApprovedQuantity());
    assertNull(item.getRequestedQuantity());
    assertNull(item.getTotalConsumedQuantity());
    assertNull(item.getTotal());
    assertNull(item.getRequestedQuantityExplanation());
    assertNull(item.getTotalStockoutDays());
    assertNull(item.getPacksToShip());
    assertNull(item.getTotalCost());
    assertNull(item.getNumberOfNewPatientsAdded());
    assertNull(item.getAdjustedConsumption());
    assertNull(item.getAverageConsumption());
    assertNull(item.getMaximumStockQuantity());
    assertNull(item.getCalculatedOrderQuantity());
    assertEquals(item.getStockAdjustments().size(), 0);
    assertEquals(item.getPreviousAdjustedConsumptions().size(), 0);
    assertNull(item.getCalculatedOrderQuantityIsa());
  }

  @Test
  public void shouldUpdatePacksToShip() {
    // given
    UUID productId = UUID.randomUUID();

    OrderableDto product = new OrderableDtoDataBuilder()
        .withId(productId)
        .withNetContent(1)
        .build();

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);
    item.setRequestedQuantity(5);
    item.setApprovedQuantity(5);
    item.setCalculatedOrderQuantity(5);
    item.setOrderableId(productId);

    // when
    item.updatePacksToShip(product);

    // then
    assertEquals(5L, item.getPacksToShip().longValue());
  }

  @Test
  public void shouldOnlyUpdateApprovedFieldsWhenRequisitionStatusIsAuthorized() {
    Requisition requisition = mockReq(RequisitionStatus.AUTHORIZED);
    assertOnlyApprovalFieldsEditable(requisition);
  }

  @Test
  public void shouldOnlyUpdateApprovedFieldsWhenRequisitionStatusIsInApproval() {
    Requisition requisition = mockReq(RequisitionStatus.IN_APPROVAL);
    assertOnlyApprovalFieldsEditable(requisition);
  }

  @Test
  public void shouldUpdateSubmissionFields() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);
    item.setOrderableId(UUID.randomUUID());
    item.setStockOnHand(1);
    item.setTotalConsumedQuantity(2);
    item.setBeginningBalance(3);
    item.setTotalReceivedQuantity(4);
    item.setRequestedQuantity(5);
    item.setTotalStockoutDays(6);
    item.setTotal(7);
    item.setNumberOfNewPatientsAdded(1);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setOrderableId(item.getOrderableId());
    updateItem.setStockOnHand(11);
    updateItem.setTotalConsumedQuantity(22);
    updateItem.setBeginningBalance(33);
    updateItem.setTotalReceivedQuantity(44);
    updateItem.setRequestedQuantity(55);
    updateItem.setTotalStockoutDays(66);
    updateItem.setTotal(77);
    updateItem.setNumberOfNewPatientsAdded(2);

    item.updateFrom(updateItem);

    assertThat(item.getStockOnHand(), is(11));
    assertThat(item.getTotalConsumedQuantity(), is(22));
    assertThat(item.getBeginningBalance(), is(33));
    assertThat(item.getTotalReceivedQuantity(), is(44));
    assertThat(item.getRequestedQuantity(), is(55));
    assertThat(item.getTotalStockoutDays(), is(66));
    assertThat(item.getTotal(), is(77));
    assertThat(item.getNumberOfNewPatientsAdded(), is(2));
  }

  @Test
  public void shouldSetSkippedAsFalseByDefault() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setSkipped(null);

    item.updateFrom(updateItem);

    assertFalse(item.getSkipped());
  }

  @Test
  public void shouldClearStockAdjustmentsIfNullWhileUpdating() {
    StockAdjustment adjustment = new StockAdjustmentDataBuilder().build();

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);
    item.setStockAdjustments(null);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setStockAdjustments(singletonList(adjustment));

    item.updateFrom(updateItem);

    assertThat(item.getStockAdjustments(), hasItems(adjustment));

    List<StockAdjustment> adjustments = new ArrayList<>();
    adjustments.add(new StockAdjustmentDataBuilder().build());
    item.setStockAdjustments(adjustments);
    item.updateFrom(updateItem);

    assertThat(item.getStockAdjustments(), hasItems(adjustment));
  }

  @Test
  public void shouldReturnApprovedQuantityWhenItIsNotNull() {
    // given
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.AUTHORIZED);
    item.setApprovedQuantity(4);

    // when
    int quantity = item.getOrderQuantity();

    // then
    assertEquals(4, quantity);
  }

  @Test
  public void shouldReturnZeroIfRequestedQuantityIsNotNullAndApprovedQuantityIsNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.AUTHORIZED);
    item.setRequestedQuantity(5);
    item.setApprovedQuantity(null);

    // when
    int quantity = item.getOrderQuantity();

    // then
    assertEquals(0, quantity);
  }

  @Test
  public void shouldReturnZeroWhenApprovedQuantityAndRequestedQuantityIsNull() {
    // given
    Requisition requisition = new Requisition();
    requisition.setTemplate(new RequisitionTemplateDataBuilder().build());

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    item.getRequisition().setStatus(RequisitionStatus.AUTHORIZED);

    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);

    // when
    int quantity = item.getOrderQuantity();

    // then
    assertEquals(0, quantity);
  }

  @Test
  public void shouldReturnCalculatedOrderQuantityIsaWhenRequisitionIsStockBasedAndItIsNotNull() {
    // given
    Requisition requisition = new Requisition();
    requisition.setTemplate(new RequisitionTemplateDataBuilder()
        .withPopulateStockOnHandFromStockCards()
        .build());

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    item.getRequisition().setStatus(RequisitionStatus.SUBMITTED);

    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);
    item.setCalculatedOrderQuantityIsa(100);

    // when
    int quantity = item.getOrderQuantity();

    // then
    assertEquals(100, quantity);
  }

  @Test
  public void shouldReturnCalculatedOrderQuantityWhenRequisitionIsNotStockBasedAndItIsNotNull() {
    // given
    Requisition requisition = new Requisition();
    requisition.setTemplate(new RequisitionTemplateDataBuilder()
        .build());

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    item.getRequisition().setStatus(RequisitionStatus.SUBMITTED);

    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);
    item.setCalculatedOrderQuantity(50);

    // when
    int quantity = item.getOrderQuantity();

    // then
    assertEquals(50, quantity);
  }

  @Test
  public void shouldNotReturnApprovedQuantityIfRequisitionIsSubmitted() {
    // given
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.SUBMITTED);

    item.setApprovedQuantity(10);
    item.setRequestedQuantity(5);

    // when
    int quantity = item.getOrderQuantity();

    // then
    assertEquals(5, quantity);
  }

  @Test
  public void shouldReturnZeroIfNotPopulatedFromStockCardsAndCalculatedIsNull() {
    Requisition requisition = new Requisition();
    requisition.setTemplate(new RequisitionTemplateDataBuilder()
        .build());

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    item.getRequisition().setStatus(RequisitionStatus.SUBMITTED);

    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);
    item.setCalculatedOrderQuantity(null);

    assertEquals(0, item.getOrderQuantity());
  }

  @Test
  public void shouldReturnZeroIfPopulatedFromStockCardsAndCalculatedIsaIsNull() {
    Requisition requisition = new Requisition();
    requisition.setTemplate(new RequisitionTemplateDataBuilder()
        .withPopulateStockOnHandFromStockCards()
        .build());

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    item.getRequisition().setStatus(RequisitionStatus.SUBMITTED);

    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);
    item.setCalculatedOrderQuantityIsa(null);

    assertEquals(0, item.getOrderQuantity());
  }

  @Test
  public void shouldBuildFromConstructorAndExport() {
    RequisitionLineItemDto lineItemDto = testConstructionAndExport(pricePerPack);

    assertThat(lineItemDto.getPricePerPack(), is(pricePerPack));
  }

  @Test
  public void shouldBuildFromConstructorAndExportWithDefaultPrice() {
    RequisitionLineItemDto lineItemDto = testConstructionAndExport(null);

    assertThat(lineItemDto.getPricePerPack(),
        is(Money.of(CurrencyUnit.USD, RequisitionLineItem.PRICE_PER_PACK_IF_NULL)));
  }

  @Test
  public void shouldReturnNumberOfNewPatientsAddedWhenItIsNotNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setNumberOfNewPatientsAdded(10);

    assertEquals(10, item.getNumberOfNewPatientsAdded().intValue());
  }

  @Test
  public void shouldReturnFalseWhenSkippedIsNotSet() {
    RequisitionLineItemDto requisitionLineItemDto = new RequisitionLineItemDto();
    RequisitionLineItem requisitionLineItem =
        RequisitionLineItem.newRequisitionLineItem(requisitionLineItemDto);
    assertEquals(false, requisitionLineItem.getSkipped());
  }

  @Test
  public void shouldNotSetPreviousAdjustedConsumption() {
    RequisitionLineItemDto requisitionLineItemDto = new RequisitionLineItemDto();
    requisitionLineItemDto.setPreviousAdjustedConsumptions(singletonList(1));
    RequisitionLineItem requisitionLineItem =
        RequisitionLineItem.newRequisitionLineItem(requisitionLineItemDto);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldSetStockAdjustments() {
    RequisitionLineItemDto requisitionLineItemDto = new RequisitionLineItemDto();
    requisitionLineItemDto.setStockAdjustments(singletonList(new StockAdjustmentDto()));
    RequisitionLineItem requisitionLineItem =
        RequisitionLineItem.newRequisitionLineItem(requisitionLineItemDto);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldSetAverageConsumption() throws Exception {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setPreviousAdjustedConsumptions(Lists.newArrayList(1, 2, 3));
    requisitionLineItem.setAdjustedConsumption(4);

    requisitionLineItem.calculateAndSetAverageConsumption();

    assertEquals(3L, requisitionLineItem.getAverageConsumption().longValue());
  }

  @Test
  public void shouldReturnFalseIfSkippedIsNull() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setSkipped(null);

    assertFalse(requisitionLineItem.isLineSkipped());
  }

  @Test
  public void shouldReturnSkippedValueIfSkippedIsNotNull() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();

    requisitionLineItem.setSkipped(true);
    assertTrue(requisitionLineItem.isLineSkipped());
    requisitionLineItem.setSkipped(false);
    assertFalse(requisitionLineItem.isLineSkipped());
  }

  @Test
  public void shouldReturnTrueIfStockOnHandIsNullForTotalConsumedQuantity() {
    RequisitionLineItem item = new RequisitionLineItemDataBuilder().setStockOnHand(null).build();
    assertTrue(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY));
  }

  @Test
  public void shouldReturnFalseIfStockOnHandIsNotNullForTotalConsumedQuantity() {
    RequisitionLineItem item = new RequisitionLineItemDataBuilder().setStockOnHand(0).build();
    assertFalse(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY));
  }

  @Test
  public void shouldReturnTrueIfTotalConsumedQuantityIsNullForStockOnHand() {
    RequisitionLineItem item = new RequisitionLineItemDataBuilder()
        .setTotalConsumedQuantity(null).build();
    assertTrue(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.STOCK_ON_HAND));
  }

  @Test
  public void shouldReturnFalseIfTotalConsumedQuantityIsNotNullForStockOnHand() {
    RequisitionLineItem item = new RequisitionLineItemDataBuilder()
        .setTotalConsumedQuantity(0).build();
    assertFalse(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.STOCK_ON_HAND));
  }

  @Test
  public void shouldReturnFalseForOtherColumns() {
    RequisitionLineItem item = new RequisitionLineItemDataBuilder().build();
    assertFalse(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.ADJUSTED_CONSUMPTION));
    assertFalse(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.APPROVED_QUANTITY));
    assertFalse(item.allRequiredCalcFieldsNotFilled(RequisitionLineItem.REMARKS_COLUMN));
    assertFalse(item.allRequiredCalcFieldsNotFilled(""));
  }

  @Test
  public void shouldThrowExceptionIfNoProductWasFound() {
    expectedException.expect(ValidationMessageException.class);
    expectedException.expectMessage(MessageKeys.CAN_NOT_FIND_PROGRAM_DETAILS_FROM_ORDERABLE);

    UUID programId = UUID.randomUUID();
    new RequisitionLineItem(
        new RequisitionDataBuilder().withProgramId(programId).build(),
        new ApprovedProductDtoDataBuilder().build());
  }

  private void checkResultsOfConstruction(RequisitionLineItem item) {
    assertEquals(initiatedRequisition, item.getRequisition());
    assertEquals(maxPeriodsOfStock, item.getMaxPeriodsOfStock().doubleValue(), 0.1);
    assertEquals(orderableId, item.getOrderableId());
  }

  private void assertOnlyApprovalFieldsEditable(Requisition requisition) {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableId(UUID.randomUUID());
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setApprovedQuantity(1);
    requisitionLineItem.setRemarks("Remarks");
    requisitionLineItem.setStockOnHand(5);

    RequisitionLineItem updatedItem = new RequisitionLineItem();
    updatedItem.setOrderableId(requisitionLineItem.getOrderableId());
    updatedItem.setRequisition(requisition);
    updatedItem.updateFrom(requisitionLineItem);

    assertEquals(1, updatedItem.getApprovedQuantity().intValue());
    assertEquals("Remarks", updatedItem.getRemarks());
    assertNull(updatedItem.getStockOnHand());
  }

  private RequisitionLineItem createDefaultRequisitionLineItem(ApprovedProductDto ftap,
      List<StockAdjustment> stockAdjustments) {
    return new RequisitionLineItemDataBuilder()
        .setRequisition(initiatedRequisition)
        .setApprovedProduct(ftap)
        .setIdealStockAmount(30)
        .setBeginningBalance(3)
        .setTotalReceivedQuantity(4)
        .setTotalLossesAndAdjustments(0)
        .setStockAdjustments(stockAdjustments)
        .setStockOnHand(1)
        .setRequestedQuantity(5)
        .setTotalConsumedQuantity(2)
        .setTotal(7)
        .setApprovedQuantity(5)
        .setTotalStockoutDays(6)
        .setRemarks("remarks")
        .setRequestedQuantityExplanation("explanation")
        .setTotalCost(Money.of(CurrencyUnit.USD, 30))
        .setNumberOfNewPatientsAdded(10)
        .build();
  }

  private ApprovedProductDto createDefaultApprovedProduct(Money pricePerPack) {
    return new ApprovedProductDtoDataBuilder()
        .withOrderable(
            new OrderableDtoDataBuilder()
            .withId(orderableId)
            .withProgramOrderable(programId, pricePerPack)
            .build())
        .withMaxPeriodsOfStock(maxPeriodsOfStock)
        .build();
  }

  private RequisitionLineItemDto testConstructionAndExport(Money pricePerPack) {
    ProgramDto program = new ProgramDto();
    program.setId(programId);

    when(initiatedRequisition.getProgramId()).thenReturn(program.getId());

    List<StockAdjustment> stockAdjustments = new ArrayList<>();
    stockAdjustments.add(new StockAdjustmentDataBuilder().build());
    stockAdjustments.add(new StockAdjustmentDataBuilder().build());
    stockAdjustments.add(new StockAdjustmentDataBuilder().build());
    stockAdjustments.add(new StockAdjustmentDataBuilder().build());

    ApprovedProductDto ftap = createDefaultApprovedProduct(pricePerPack);
    RequisitionLineItem requisitionLineItem = createDefaultRequisitionLineItem(
        ftap, stockAdjustments
    );
    ProgramOrderableDto programOrderable = ftap.getOrderable().findProgramOrderableDto(programId);
    assert programOrderable != null;
    OrderableDto orderableDto = generateOrderableDto(program, programOrderable);

    RequisitionLineItemDto dto = new RequisitionLineItemDto();
    requisitionLineItem.export(dto, orderableDto);

    assertNotNull(dto);
    assertThat(dto.getOrderable().getId(), is(requisitionLineItem.getOrderableId()));
    assertThat(dto.getId(), is(requisitionLineItem.getId()));
    assertThat(dto.getBeginningBalance(), is(requisitionLineItem.getBeginningBalance()));
    assertThat(dto.getTotalReceivedQuantity(), is(requisitionLineItem.getTotalReceivedQuantity()));
    assertThat(dto.getTotalLossesAndAdjustments(),
        is(requisitionLineItem.getTotalLossesAndAdjustments()));
    assertThat(dto.getStockOnHand(), is(requisitionLineItem.getStockOnHand()));
    assertThat(dto.getRequestedQuantity(), is(requisitionLineItem.getRequestedQuantity()));
    assertThat(dto.getTotalConsumedQuantity(), is(requisitionLineItem.getTotalConsumedQuantity()));
    assertThat(dto.getTotal(), is(requisitionLineItem.getTotal()));
    assertThat(dto.getApprovedQuantity(), is(requisitionLineItem.getApprovedQuantity()));
    assertThat(dto.getTotalStockoutDays(), is(requisitionLineItem.getTotalStockoutDays()));
    assertThat(dto.getRemarks(), is(requisitionLineItem.getRemarks()));
    assertThat(dto.getRequestedQuantityExplanation(),
        is(requisitionLineItem.getRequestedQuantityExplanation()));
    assertThat(dto.getTotalCost(), is(requisitionLineItem.getTotalCost()));
    assertThat(dto.getNumberOfNewPatientsAdded(),
        is(requisitionLineItem.getNumberOfNewPatientsAdded()));
    assertThat(dto.getCalculatedOrderQuantityIsa(),
        is(requisitionLineItem.getCalculatedOrderQuantityIsa()));

    StockAdjustment.Importer[] items = stockAdjustments
        .stream()
        .map(item -> {
          StockAdjustmentDto container = new StockAdjustmentDto();
          item.export(container);

          return (StockAdjustment.Importer) container;
        }).toArray(StockAdjustment.Importer[]::new);

    assertThat(dto.getStockAdjustments(), hasSize(items.length));
    assertThat(dto.getStockAdjustments(), hasItems(items));

    return dto;
  }

  private OrderableDto generateOrderableDto(ProgramDto program,
                                            ProgramOrderableDto programOrderableDto) {
    OrderableDto orderableDto = new OrderableDto();
    orderableDto.setId(orderableId);
    program.setId(UUID.randomUUID());
    programOrderableDto.setProgramId(program.getId());
    Set<ProgramOrderableDto> products = new HashSet<>();
    products.add(programOrderableDto);
    orderableDto.setPrograms(products);
    return orderableDto;
  }

  private Requisition mockReq(RequisitionStatus status) {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(status);
    when(requisition.isApprovable()).thenReturn(status.duringApproval());
    when(requisition.getProgramId()).thenReturn(programId);
    return requisition;
  }
}
