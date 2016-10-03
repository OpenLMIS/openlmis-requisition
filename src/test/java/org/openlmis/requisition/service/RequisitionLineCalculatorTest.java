package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;


@SuppressWarnings({"PMD.TooManyMethods"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionLineCalculatorTest {

  private static final String BEGINNING_BALANCE_FIELD = "beginningBalance";
  private static final String TOTAL_QUANTITY_RECEIVED_FIELD = "totalQuantityReceived";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  private Requisition requisition;
  private RequisitionLineItem requisitionLineItem;
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
  private ProcessingPeriodDto periodDto;

  @InjectMocks
  private RequisitionLineCalculator requisitionLineCalculator;

  @Mock
  private AvailableRequisitionColumn availableRequisitionColumn;

  private UUID program = UUID.randomUUID();
  private UUID period = UUID.randomUUID();
  private UUID productId = UUID.randomUUID();

  @Before
  public void setUp() {
    generateInstances();
    mockRepositories();
  }

  @Test
  public void shouldInitiateRequisitionLineItemFieldsIfValidRequisitionProvided() {
    final Integer expectedBeginningBalance = 20;
    final Integer expectedTotalReceivedQuantity = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, false, true, true, SOURCE,
        availableRequisitionColumn));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineCalculator
        .initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = requisitionWithInitiatedLines
        .getRequisitionLineItems().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLineItem.getBeginningBalance());
    assertEquals(expectedTotalReceivedQuantity, requisitionLineItem.getTotalReceivedQuantity());
  }

  @Test
  public void shouldNotInitiateBeginningBalanceWhenItIsNotDisplayed() {
    final Integer expectedBeginningBalance = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, false, false, true, true, SOURCE,
        availableRequisitionColumn));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineCalculator
        .initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = requisitionWithInitiatedLines
        .getRequisitionLineItems().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLineItem.getBeginningBalance());
  }

  @Test
  public void shouldDisplayColumnsInCorrectOrder() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
        new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD,
            2, false, false, true, true, SourceType.USER_INPUT, availableRequisitionColumn));
    requisitionTemplateColumnHashMap.put(TOTAL_QUANTITY_RECEIVED_FIELD,
        new RequisitionTemplateColumn(TOTAL_QUANTITY_RECEIVED_FIELD, TOTAL_QUANTITY_RECEIVED_FIELD,
            1, false, false, true, true, SourceType.USER_INPUT, availableRequisitionColumn));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLineCalculator.initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionTemplate requisitionTemplateList
        = requisitionTemplateService.getTemplateForProgram(requisition.getProgram());

    Map<String, RequisitionTemplateColumn> testRequisitionTemplateColumnHashMap
        = requisitionTemplateList.getColumnsMap();

    assertEquals(1, testRequisitionTemplateColumnHashMap
        .get(TOTAL_QUANTITY_RECEIVED_FIELD).getDisplayOrder());
    assertEquals(2, testRequisitionTemplateColumnHashMap
        .get(BEGINNING_BALANCE_FIELD).getDisplayOrder());
  }

  @Test
  public void shouldFindRequisitionLineItemIfItExists() {
    List<RequisitionLineItem> receivedRequisitionLineItems =
        requisitionLineCalculator.searchRequisitionLineItems(
        requisition, productId);

    assertEquals(1, receivedRequisitionLineItems.size());
    assertEquals(requisitionLineItem, receivedRequisitionLineItems.get(0));
  }

  private void generateInstances() {
    requisition = createTestRequisition(UUID.randomUUID(), period, program,
        RequisitionStatus.INITIATED);
    requisitionLineItem = createTestRequisitionLineItem(UUID.randomUUID(), 10, 20, requisition);

    requisition.setRequisitionLineItems(new ArrayList<>(
            Collections.singletonList(requisitionLineItem)));
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
  }

  private Requisition createTestRequisition(UUID facility, UUID period,
                                            UUID program,
                                            RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(requisitionStatus);
    return requisition;
  }

  private RequisitionLineItem createTestRequisitionLineItem(UUID product, Integer quantityRequested,
                                                            Integer stockInHand,
                                                            Requisition requisition) {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setOrderableProduct(product);
    requisitionLineItem.setRequestedQuantity(quantityRequested);
    requisitionLineItem.setStockInHand(stockInHand);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProduct(productId);
    return requisitionLineItem;
  }

  private void mockRepositories() {
    when(requisitionTemplateService
        .getTemplateForProgram(program))
        .thenReturn(requisitionTemplate);
    when(periodReferenceDataService
        .search(any(), any()))
        .thenReturn(Collections.singletonList(periodDto));
    when(requisitionService
        .searchRequisitions(eq(requisition.getFacility()), eq(requisition.getProgram()),
            eq(null), eq(null), any(), eq(null), eq(null)))
        .thenReturn(Collections.singletonList(requisition));
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(new ProgramDto());
    when(periodReferenceDataService
        .findOne(any()))
        .thenReturn(periodDto);
    when(periodDto.getProcessingSchedule())
        .thenReturn(new ProcessingScheduleDto());
  }
}
