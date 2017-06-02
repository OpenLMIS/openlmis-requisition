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

package org.openlmis.utils;

import static java.util.Collections.singletonList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;


@SuppressWarnings({"PMD.TooManyMethods"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionExportHelperTest {

  private static final Money PRICE_PER_PACK = Money.of(CurrencyUnit.USD, 9);
  private static final long PACK_SIZE = 2;

  private Requisition requisition;
  private OrderableDto orderableDto;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private ProcessingPeriodDto periodDto1;

  @Mock
  private ProcessingPeriodDto periodDto2;

  @Mock
  private ProcessingPeriodDto periodDto3;

  @Mock
  private OrderableReferenceDataService orderableReferenceDataService;

  @InjectMocks
  private RequisitionExportHelper requisitionExportHelper;

  private UUID program = UUID.randomUUID();
  private UUID period1 = UUID.randomUUID();
  private UUID period2 = UUID.randomUUID();
  private UUID period3 = UUID.randomUUID();
  private UUID productId = UUID.randomUUID();

  @Before
  public void setUp() {
    generateInstances();
    mockRepositories();
  }

  @Test
  public void shouldExportRequisitionLinesToDtos() {
    RequisitionLineItem requisitionLineItem =
        generateRequisitionLineItemToExport(orderableDto.getId());
    List<RequisitionLineItemDto> items =
        requisitionExportHelper.exportToDtos(singletonList(requisitionLineItem));
    RequisitionLineItemDto item = items.get(0);
    assertNotNull(item);
    assertEquals(item.getId(), requisitionLineItem.getId());
    assertEquals(item.getOrderable().getId(), requisitionLineItem.getOrderableId());
    assertEquals(item.getBeginningBalance(), requisitionLineItem.getBeginningBalance());
    assertEquals(item.getTotalReceivedQuantity(), requisitionLineItem.getTotalReceivedQuantity());
    assertEquals(item.getTotalLossesAndAdjustments(),
        requisitionLineItem.getTotalLossesAndAdjustments());
    assertEquals(item.getStockOnHand(), requisitionLineItem.getStockOnHand());
    assertEquals(item.getRequestedQuantity(), requisitionLineItem.getRequestedQuantity());
    assertEquals(item.getTotalConsumedQuantity(), requisitionLineItem.getTotalConsumedQuantity());
    assertEquals(item.getRequestedQuantityExplanation(),
        requisitionLineItem.getRequestedQuantityExplanation());
    assertEquals(item.getRemarks(), requisitionLineItem.getRemarks());
    assertEquals(item.getApprovedQuantity(), requisitionLineItem.getApprovedQuantity());
    assertEquals(item.getTotalStockoutDays(), requisitionLineItem.getTotalStockoutDays());
    assertEquals(item.getTotal(), requisitionLineItem.getTotal());
    assertEquals(PRICE_PER_PACK, item.getPricePerPack());
    assertEquals(item.getNumberOfNewPatientsAdded(),
        requisitionLineItem.getNumberOfNewPatientsAdded());
  }

  private RequisitionLineItem generateRequisitionLineItemToExport(UUID orderableDtoUuid) {
    ProgramOrderableDto programOrderableDto = new ProgramOrderableDto();
    programOrderableDto.setProgramId(program);
    Set<ProgramOrderableDto> products = new HashSet<>();
    products.add(programOrderableDto);
    orderableDto.setPrograms(products);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableId(orderableDtoUuid);
    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setBeginningBalance(3);
    requisitionLineItem.setTotalReceivedQuantity(4);
    requisitionLineItem.setTotalLossesAndAdjustments(0);
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setRequestedQuantity(5);
    requisitionLineItem.setTotalConsumedQuantity(2);
    requisitionLineItem.setTotal(7);
    requisitionLineItem.setApprovedQuantity(5);
    requisitionLineItem.setTotalStockoutDays(6);
    requisitionLineItem.setPricePerPack(PRICE_PER_PACK);
    requisitionLineItem.setNumberOfNewPatientsAdded(8);

    return requisitionLineItem;
  }

  private void generateInstances() {
    requisition = createTestRequisition(UUID.randomUUID(), period1, program,
        RequisitionStatus.INITIATED);
    RequisitionLineItem requisitionLineItem = createTestRequisitionLineItem(
        10, 20, requisition
    );

    requisition.setRequisitionLineItems(new ArrayList<>(
        singletonList(requisitionLineItem)));
    orderableDto = new OrderableDto();
    orderableDto.setId(UUID.randomUUID());
    orderableDto.setNetContent(PACK_SIZE);
  }

  private Requisition createTestRequisition(UUID facility, UUID period,
                                            UUID program, RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition(facility, program, period, requisitionStatus, false);
    requisition.setId(UUID.randomUUID());
    return requisition;
  }

  private RequisitionLineItem createTestRequisitionLineItem(Integer quantityRequested,
                                                            Integer stockOnHand,
                                                            Requisition requisition) {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setRequestedQuantity(quantityRequested);
    requisitionLineItem.setStockOnHand(stockOnHand);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableId(productId);
    requisitionLineItem.setPricePerPack(PRICE_PER_PACK);
    return requisitionLineItem;
  }

  private void mockRepositories() {
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(new ProgramDto());
    when(periodDto1.getProcessingSchedule())
        .thenReturn(new ProcessingScheduleDto());
    when(periodDto1.getStartDate())
        .thenReturn(LocalDate.of(2016, Month.MARCH, 10));
    when(periodDto2.getStartDate())
        .thenReturn(LocalDate.of(2016, Month.FEBRUARY, 10));
    when(periodDto3.getStartDate())
        .thenReturn(LocalDate.of(2016, Month.JANUARY, 10));
    when(periodDto1.getId())
        .thenReturn(period1);
    when(periodDto2.getId())
        .thenReturn(period2);
    when(periodDto3.getId())
        .thenReturn(period3);
    when(orderableReferenceDataService.findOne(orderableDto.getId()))
        .thenReturn(orderableDto);
  }
}
