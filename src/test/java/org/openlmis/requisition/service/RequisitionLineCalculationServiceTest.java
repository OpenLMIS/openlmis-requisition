package org.openlmis.requisition.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;

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
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
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

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private RequisitionTemplateService requisitionTemplateService;


  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private ProcessingPeriodDto periodDto1;

  @Mock
  private ProcessingPeriodDto periodDto2;

  @Mock
  private ProcessingPeriodDto periodDto3;

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
                                                            Integer stockInHand,
                                                            Requisition requisition) {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setRequestedQuantity(quantityRequested);
    requisitionLineItem.setStockInHand(stockInHand);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(productId);
    return requisitionLineItem;
  }

  private void mockRepositories() {
    when(requisitionTemplateService
        .getTemplateForProgram(program))
        .thenReturn(requisitionTemplate);
    when(periodReferenceDataService
        .search(any(), any()))
        .thenReturn(Lists.newArrayList(periodDto3, periodDto2, periodDto1));
    when(requisitionService
        .searchRequisitions(requisition.getFacilityId(), requisition.getProgramId(),
            null, null, period2, null, null))
        .thenReturn(Collections.singletonList(requisition));
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(new ProgramDto());
    when(periodReferenceDataService
        .findOne(any()))
        .thenReturn(periodDto1);
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
  }
}
