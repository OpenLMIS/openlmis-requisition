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

package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.DatePhysicalStockCountCompleted;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.ReasonType;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.utils.RequisitionExportHelper;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionDtoBuilderTest {

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private PeriodService periodService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private RequisitionExportHelper requisitionExportHelper;

  @Mock
  private RequisitionLineItem requisitionLineItem;
  @Mock
  private RequisitionLineItemDto requisitionLineItemDto;

  @Mock
  private OrderableReferenceDataService orderableReferenceDataService;

  @InjectMocks
  private RequisitionDtoBuilder requisitionDtoBuilder = new RequisitionDtoBuilder();


  private FacilityDto facility = DtoGenerator.of(FacilityDto.class);
  private ProcessingPeriodDto processingPeriod = DtoGenerator.of(ProcessingPeriodDto.class);
  private ProgramDto program = DtoGenerator.of(ProgramDto.class);

  private Requisition requisition;
  private StockAdjustmentReason stockAdjustmentReason;

  private List<RequisitionLineItemDto> lineItemDtos = new ArrayList<>();

  private UUID requisitionUuid = UUID.randomUUID();
  private UUID supervisoryNodeUuid = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();

  private OrderableDto orderableDto;

  @Before
  public void setUp() {
    lineItemDtos = new ArrayList<>();
    lineItemDtos.add(requisitionLineItemDto);

    requisition = buildRequisition();
    orderableDto = new OrderableDtoDataBuilder()
        .withId(orderableId)
        .withProgramOrderable(program.getId(), false)
        .build();
  }

  @Test
  public void shouldBuildDtoFromRequisition() {
    when(facilityReferenceDataService.findOne(facility.getId())).thenReturn(facility);
    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodService.getPeriod(processingPeriod.getId())).thenReturn(processingPeriod);
    when(requisitionExportHelper.exportToDtos(
        Collections.singletonList(requisitionLineItem),
        ImmutableMap.of(orderableId, orderableDto), false))
        .thenReturn(lineItemDtos);
    when(orderableReferenceDataService
        .findByIds(requisition.getAllOrderableIds()))
        .thenReturn(Collections.singletonList(orderableDto));

    RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);

    assertNotNull(requisitionDto);
    assertEquals(requisition.getId(), requisitionDto.getId());
    assertEquals(requisition.getSupervisoryNodeId(), requisitionDto.getSupervisoryNode());
    assertEquals(requisition.getEmergency(), requisitionDto.getEmergency());
    assertEquals(facility, requisitionDto.getFacility());
    assertEquals(program, requisitionDto.getProgram());
    assertEquals(processingPeriod, requisitionDto.getProcessingPeriod());
    assertEquals(requisition.getModifiedDate(), requisitionDto.getModifiedDate());
    assertEquals(lineItemDtos, requisitionDto.getRequisitionLineItems());
    assertEquals(
        requisition.getDatePhysicalStockCountCompleted().getLocalDate(),
        requisitionDto.getDatePhysicalStockCountCompleted());
    assertEquals(requisition.getStatus(), requisitionDto.getStatus());
    assertNotNull(requisitionDto.getModifiedDate());
    assertEquals(Collections.singleton(orderableDto),
        requisitionDto.getAvailableNonFullSupplyProducts());

    assertReasonsEquals(requisitionDto.getStockAdjustmentReasons());
  }

  @Test
  public void shouldPopulateAvailableProductsCollectionsBasedOnFullSupplyFlag() {
    OrderableDto fs1 = new OrderableDtoDataBuilder()
        .withProgramOrderable(requisition.getProgramId(), true)
        .build();
    OrderableDto fs2 = new OrderableDtoDataBuilder()
        .withProgramOrderable(requisition.getProgramId(), true)
        .build();
    OrderableDto nfs1 = new OrderableDtoDataBuilder()
        .withProgramOrderable(requisition.getProgramId(), false)
        .build();
    OrderableDto nfs2 = new OrderableDtoDataBuilder()
        .withProgramOrderable(requisition.getProgramId(), false)
        .build();
    OrderableDto differentProgram = new OrderableDtoDataBuilder()
        .withProgramOrderable(UUID.randomUUID(), true)
        .build();

    when(orderableReferenceDataService
        .findByIds(anySetOf(UUID.class)))
        .thenReturn(Lists.newArrayList(fs1, fs2, nfs1, nfs2, differentProgram));

    RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);

    assertNotNull(requisitionDto);
    assertNotNull(requisitionDto.getAvailableFullSupplyProducts());
    assertNotNull(requisitionDto.getAvailableNonFullSupplyProducts());
    assertEquals(2, requisitionDto.getAvailableFullSupplyProducts().size());
    assertEquals(2, requisitionDto.getAvailableNonFullSupplyProducts().size());
    assertTrue(requisitionDto.getAvailableFullSupplyProducts().contains(fs1));
    assertTrue(requisitionDto.getAvailableFullSupplyProducts().contains(fs2));
    assertTrue(requisitionDto.getAvailableNonFullSupplyProducts().contains(nfs1));
    assertTrue(requisitionDto.getAvailableNonFullSupplyProducts().contains(nfs2));
  }

  @Test
  public void shouldBuildBatchDtoFromRequisition() {
    Map<UUID, OrderableDto> orderables = Collections.singletonMap(UUID.randomUUID(), orderableDto);
    when(requisitionExportHelper
        .exportToDtos(Collections.singletonList(requisitionLineItem), orderables, true))
        .thenReturn(lineItemDtos);

    RequisitionDto requisitionDto =
        requisitionDtoBuilder
            .buildBatch(requisition, facility,
                orderables, processingPeriod);

    assertNotNull(requisitionDto);
    assertEquals(requisition.getId(), requisitionDto.getId());
    assertEquals(requisition.getEmergency(), requisitionDto.getEmergency());
    assertEquals(facility, requisitionDto.getFacility());
    assertEquals(processingPeriod, requisitionDto.getProcessingPeriod());
    assertEquals(requisition.getModifiedDate(), requisitionDto.getModifiedDate());
    assertEquals(lineItemDtos, requisitionDto.getRequisitionLineItems());
    assertEquals(requisition.getStatus(), requisitionDto.getStatus());
    assertNotNull(requisitionDto.getModifiedDate());
    assertEquals(requisition.getModifiedDate(), requisitionDto.getModifiedDate());
    assertEquals(requisition.getSupervisoryNodeId(), requisitionDto.getSupervisoryNode());

    assertNull(requisitionDto.getProgram());
    assertNull(requisitionDto.getStockAdjustmentReasons());
    assertNull(requisitionDto.getAvailableNonFullSupplyProducts());
  }

  @Test
  public void shouldBuildDtoFromRequisitionWhenReferenceDataInstancesDoNotExist() {
    when(facilityReferenceDataService.findOne(facility.getId())).thenReturn(null);
    when(programReferenceDataService.findOne(program.getId())).thenReturn(null);
    when(periodService.getPeriod(processingPeriod.getId())).thenReturn(null);

    RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);

    verify(requisitionExportHelper)
        .exportToDtos(anyListOf(RequisitionLineItem.class), anyMap(), eq(false));

    assertNotNull(requisitionDto);
    assertNull(requisitionDto.getFacility());
    assertNull(requisitionDto.getProgram());
    assertNull(requisitionDto.getProcessingPeriod());
  }

  private Requisition buildRequisition() {
    Requisition requisition = new Requisition(facility.getId(), program.getId(),
        processingPeriod.getId(), RequisitionStatus.INITIATED, false);
    requisition.setId(requisitionUuid);
    requisition.setSupervisoryNodeId(supervisoryNodeUuid);
    RequisitionTemplate template = new RequisitionTemplate();
    template.setId(UUID.randomUUID());
    requisition.setTemplate(template);
    requisition.setModifiedDate(ZonedDateTime.now());
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));
    requisition.setDatePhysicalStockCountCompleted(
        new DatePhysicalStockCountCompleted(LocalDate.now()));
    requisition.setAvailableProducts(Collections.singleton(orderableId));

    StockAdjustmentReason reason = generateStockAdjustmentReason();
    requisition.setStockAdjustmentReasons(Collections.singletonList(reason));

    return requisition;
  }

  private StockAdjustmentReason generateStockAdjustmentReason() {
    stockAdjustmentReason = new StockAdjustmentReason();
    stockAdjustmentReason.setReasonId(UUID.randomUUID());
    stockAdjustmentReason.setReasonType(ReasonType.CREDIT);
    stockAdjustmentReason.setReasonCategory(ReasonCategory.ADJUSTMENT);
    stockAdjustmentReason.setDescription("description");
    stockAdjustmentReason.setIsFreeTextAllowed(false);
    return stockAdjustmentReason;
  }

  private void assertReasonsEquals(List<ReasonDto> reasonDtos) {
    ReasonDto reasonDto = reasonDtos.get(0);
    assertEquals(stockAdjustmentReason.getReasonId(), reasonDto.getId());
    assertEquals(stockAdjustmentReason.getDescription(), reasonDto.getDescription());
    assertEquals(stockAdjustmentReason.getReasonType(), reasonDto.getReasonType());
    assertEquals(stockAdjustmentReason.getReasonCategory(), reasonDto.getReasonCategory());
    assertEquals(stockAdjustmentReason.getIsFreeTextAllowed(), reasonDto.getIsFreeTextAllowed());
  }
}
