package org.openlmis.requisition.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramProductDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;


@SuppressWarnings({"PMD.TooManyMethods"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionLineCalculationServiceTest {

  private static final String BEGINNING_BALANCE_FIELD = "beginningBalance";
  private static final String BEGINNING_BALANCE_INDICATOR = "A";
  private static final String TOTAL_QUANTITY_RECEIVED_FIELD = "totalQuantityReceived";
  private static final String TOTAL_QUANTITY_RECEIVED_INDICATOR = "B";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  private Requisition requisition;
  private RequisitionTemplate requisitionTemplate;
  private OrderableProductDto orderableProductDto;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private RequisitionTemplateService requisitionTemplateService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodService periodService;

  @Mock
  private ProcessingPeriodDto periodDto1;

  @Mock
  private ProcessingPeriodDto periodDto2;

  @Mock
  private ProcessingPeriodDto periodDto3;

  @Mock
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  @InjectMocks
  private RequisitionLineCalculationService requisitionLineCalculationService;

  @Mock
  private AvailableRequisitionColumn availableRequisitionColumn;

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
  public void shouldInitiateRequisitionLineItemFieldsIfValidRequisitionProvided()
      throws RequisitionTemplateColumnException {
    final Integer expectedBeginningBalance = 20;
    final Integer expectedTotalReceivedQuantity = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_INDICATOR, 1, true, 
        SOURCE, availableRequisitionColumn));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineCalculationService
        .initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = requisitionWithInitiatedLines
        .getRequisitionLineItems().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLineItem.getBeginningBalance());
    assertEquals(expectedTotalReceivedQuantity, requisitionLineItem.getTotalReceivedQuantity());
  }

  @Test
  public void shouldNotInitiateBeginningBalanceWhenItIsNotDisplayed()
      throws RequisitionTemplateColumnException {
    final Integer expectedBeginningBalance = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_INDICATOR, 1, false, 
        SOURCE, availableRequisitionColumn));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineCalculationService
        .initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = requisitionWithInitiatedLines
        .getRequisitionLineItems().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLineItem.getBeginningBalance());
  }

  @Test
  public void shouldDisplayColumnsInCorrectOrder() throws RequisitionTemplateColumnException {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
        new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 
            BEGINNING_BALANCE_INDICATOR, 2, false,  SourceType.USER_INPUT, 
            availableRequisitionColumn));
    requisitionTemplateColumnHashMap.put(TOTAL_QUANTITY_RECEIVED_FIELD,
        new RequisitionTemplateColumn(TOTAL_QUANTITY_RECEIVED_FIELD, TOTAL_QUANTITY_RECEIVED_FIELD, 
            TOTAL_QUANTITY_RECEIVED_INDICATOR, 1, false, SourceType.USER_INPUT, 
            availableRequisitionColumn));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLineCalculationService.initiateRequisitionLineItemFields(
            requisition, requisitionTemplate);

    RequisitionTemplate requisitionTemplateList
        = requisitionTemplateService.getTemplateForProgram(requisition.getProgramId());

    Map<String, RequisitionTemplateColumn> testRequisitionTemplateColumnHashMap
        = requisitionTemplateList.getColumnsMap();

    assertEquals(1, testRequisitionTemplateColumnHashMap
        .get(TOTAL_QUANTITY_RECEIVED_FIELD).getDisplayOrder());
    assertEquals(2, testRequisitionTemplateColumnHashMap
        .get(BEGINNING_BALANCE_FIELD).getDisplayOrder());
  }

  @Test
  public void shouldExportRequisitionLinesToDtos() {
    RequisitionLineItem requisitionLineItem =
        generateRequisitionLineItemToExport(orderableProductDto.getId());
    List<RequisitionLineItemDto> items =
        requisitionLineCalculationService.exportToDtos(Arrays.asList(requisitionLineItem));
    RequisitionLineItemDto item = items.get(0);
    assertNotNull(item);
    assertEquals(item.getId(), requisitionLineItem.getId());
    assertEquals(item.getOrderableProduct().getId(), requisitionLineItem.getOrderableProductId());
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
    assertNotNull(item.getPricePerPack());
  }

  private RequisitionLineItem generateRequisitionLineItemToExport(UUID orderableProductDtoUuid) {
    ProgramProductDto programProductDto = new ProgramProductDto();
    programProductDto.setProductId(orderableProductDto.getId());
    programProductDto.setProgramId(program);
    Set<ProgramProductDto> programs = new HashSet<>();
    programs.add(programProductDto);
    orderableProductDto.setPrograms(programs);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(orderableProductDtoUuid);
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
    return requisitionLineItem;
  }

  private void generateInstances() {
    requisition = createTestRequisition(UUID.randomUUID(), period1, program,
        RequisitionStatus.INITIATED);
    RequisitionLineItem requisitionLineItem = createTestRequisitionLineItem(
        10, 20, requisition
    );

    requisition.setRequisitionLineItems(new ArrayList<>(
            Collections.singletonList(requisitionLineItem)));
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgramId(program);
    orderableProductDto = new OrderableProductDto();
    orderableProductDto.setId(UUID.randomUUID());
  }

  private Requisition createTestRequisition(UUID facility, UUID period,
                                            UUID program,
                                            RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacilityId(facility);
    requisition.setProcessingPeriodId(period);
    requisition.setProgramId(program);
    requisition.setStatus(requisitionStatus);
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
    requisitionLineItem.setOrderableProductId(productId);
    return requisitionLineItem;
  }

  private void mockRepositories() {
    when(requisitionTemplateService
        .getTemplateForProgram(program))
        .thenReturn(requisitionTemplate);
    when(periodService
        .findPreviousPeriod(any()))
        .thenReturn(periodDto2);
    when(requisitionService
        .searchRequisitions(requisition.getFacilityId(), requisition.getProgramId(),
            null, null, period2, null, null, null))
        .thenReturn(Collections.singletonList(requisition));
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
    when(availableRequisitionColumn.getCanChangeOrder()).thenReturn(true);
    when(availableRequisitionColumn.getCanBeChangedByUser()).thenReturn(true);
    when(orderableProductReferenceDataService.findOne(orderableProductDto.getId()))
        .thenReturn(orderableProductDto);
  }
}
