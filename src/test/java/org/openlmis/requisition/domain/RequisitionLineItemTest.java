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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;

import com.google.common.collect.Sets;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineItemTest {

  private Requisition initiatedRequisition;

  private UUID programId = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();

  @Before
  public void setUp() {
    initiatedRequisition = mockReq(RequisitionStatus.INITIATED);
  }

  @Test
  public void shouldResetData() {
    RequisitionLineItem item = createDefaultRequisitionLineItem(createDefaultApprovedProduct(null));
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
  }

  @Test
  public void shouldUpdatePacksToShip() {
    // given
    UUID productId = UUID.randomUUID();
    long packsToShip = 5L;

    OrderableDto product = mock(OrderableDto.class);
    when(product.packsToOrder(anyLong())).thenReturn(packsToShip);
    when(product.getId()).thenReturn(productId);

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);
    item.setOrderableId(productId);

    // when
    item.updatePacksToShip(Collections.singletonList(product));

    // then
    assertEquals(item.getPacksToShip().longValue(), packsToShip);
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

    item.setStockOnHand(1);
    item.setTotalConsumedQuantity(2);
    item.setBeginningBalance(3);
    item.setTotalReceivedQuantity(4);
    item.setRequestedQuantity(5);
    item.setTotalStockoutDays(6);
    item.setTotal(7);
    item.setNumberOfNewPatientsAdded(1);

    RequisitionLineItem updateItem = new RequisitionLineItem();
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
  public void shouldNotUpdateProduct() {
    final UUID product1 = UUID.randomUUID();
    final UUID product2 = UUID.randomUUID();

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);
    item.setOrderableId(product1);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setRequisition(initiatedRequisition);
    updateItem.setOrderableId(product2);

    item.updateFrom(updateItem);

    assertThat(item.getOrderableId(), is(product1));
  }

  @Test
  public void shouldReturnApprovedQuantityWhenItIsNotNull() {
    // given
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.AUTHORIZED);
    item.setApprovedQuantity(4);

    // when
    Integer quantity = item.getOrderQuantity();

    // then
    assertEquals(4, quantity.intValue());
  }

  @Test
  public void shouldReturnRequestedQuantityWhenItIsNotNullAndApprovedQuantityIsNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.AUTHORIZED);
    item.setRequestedQuantity(5);
    item.setApprovedQuantity(null);

    // when
    Integer quantity = item.getOrderQuantity();

    // then
    assertEquals(5, quantity.intValue());
  }

  @Test
  public void shouldReturnZeroWhenApprovedQuantityAndRequestedQuantityIsNull() {
    // given
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.AUTHORIZED);

    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);

    // when
    Integer quantity = item.getOrderQuantity();

    // then
    assertEquals(0, quantity.intValue());
  }

  @Test
  public void shouldNotReturnApprovedQuantityIfRequisitionIsPreAuthorized() {
    // given
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(new Requisition());
    item.getRequisition().setStatus(RequisitionStatus.SUBMITTED);

    item.setApprovedQuantity(10);
    item.setRequestedQuantity(5);

    // when
    Integer quantity = item.getOrderQuantity();

    // then
    assertEquals(5, quantity.intValue());
  }

  @Test
  public void shouldBuildFromConstructorAndExport() {
    Money pricePerPack = Money.of(CurrencyUnit.USD, 5.79);
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
    requisitionLineItemDto.setPreviousAdjustedConsumptions(Collections.singletonList(1));
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

    assertFalse(requisitionLineItem.isSkipped());
  }

  @Test
  public void shouldReturnSkippedValueIfSkippedIsNotNull() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();

    requisitionLineItem.setSkipped(true);
    assertTrue(requisitionLineItem.isSkipped());
    requisitionLineItem.setSkipped(false);
    assertFalse(requisitionLineItem.isSkipped());
  }

  private void assertOnlyApprovalFieldsEditable(Requisition requisition) {
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

  private RequisitionLineItem createDefaultRequisitionLineItem(ApprovedProductDto ftap) {
    RequisitionLineItem item =
        new RequisitionLineItem(initiatedRequisition, ftap);

    item.setId(UUID.randomUUID());
    item.setBeginningBalance(3);
    item.setTotalReceivedQuantity(4);
    item.setTotalLossesAndAdjustments(0);
    item.setStockOnHand(1);
    item.setRequestedQuantity(5);
    item.setTotalConsumedQuantity(2);
    item.setTotal(7);
    item.setApprovedQuantity(5);
    item.setTotalStockoutDays(6);
    item.setRemarks("remarks");
    item.setRequestedQuantityExplanation("explanation");
    item.setTotalCost(Money.of(CurrencyUnit.USD, 30));
    item.setNumberOfNewPatientsAdded(10);
    item.setOrderableId(orderableId);

    return item;
  }

  private ApprovedProductDto createDefaultApprovedProduct(Money pricePerPack) {
    ProgramOrderableDto programOrderable = new ProgramOrderableDto();
    programOrderable.setPricePerPack(pricePerPack);
    programOrderable.setProgramId(programId);
    OrderableDto orderable = new OrderableDto();
    orderable.setId(orderableId);
    orderable.setPrograms(Sets.newHashSet(programOrderable));
    ApprovedProductDto ftap = new ApprovedProductDto();
    ProgramDto programMock = mock(ProgramDto.class);
    when(programMock.getId()).thenReturn(programId);
    ftap.setProgram(programMock);
    ftap.setOrderable(orderable);
    ftap.setMaxPeriodsOfStock(7.25);

    return ftap;
  }

  private RequisitionLineItemDto testConstructionAndExport(Money pricePerPack) {
    ApprovedProductDto ftap = createDefaultApprovedProduct(pricePerPack);

    ProgramOrderableDto programOrderable = ftap.getOrderable().findProgramOrderableDto(programId);
    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());

    when(initiatedRequisition.getProgramId()).thenReturn(program.getId());

    RequisitionLineItem requisitionLineItem = createDefaultRequisitionLineItem(ftap);
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
    return requisition;
  }
}
