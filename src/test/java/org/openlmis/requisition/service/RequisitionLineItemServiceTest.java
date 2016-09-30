package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.util.ArrayList;
import java.util.Arrays;
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
public class RequisitionLineItemServiceTest {

  private static final String BEGINNING_BALANCE_FIELD = "beginningBalance";
  private static final String TOTAL_QUANTITY_RECEIVED_FIELD = "totalQuantityReceived";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  private Requisition requisition;
  private RequisitionLineItem requisitionLineItem;
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private RequisitionLineItemRepository requisitionLineItemRepository;

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
  private RequisitionLineItemService requisitionLineItemService;

  private UUID program;
  private UUID period;

  @Before
  public void setUp() {
    program = UUID.randomUUID();
    period = UUID.randomUUID();
    generateInstances();
    mockRepositories();
  }

  @Test
  public void shouldInitiateRequisitionLineItemFieldsIfValidRequisitionProvided() {
    final Integer expectedBeginningBalance = 20;
    final Integer expectedTotalReceivedQuantity = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, false, true, true, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineItemService
        .initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = requisitionWithInitiatedLines
        .getRequisitionLineItems().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLineItem.getBeginningBalance());
    assertEquals(expectedTotalReceivedQuantity, requisitionLineItem.getTotalReceivedQuantity());
  }

  @Test
  public void shouldResetBeginningBalanceWhenSavingRequisitionLineItem()
      throws RequisitionException {
    final Integer expectedBeginningBalance = 20;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, true, true, false, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLineItem.setBeginningBalance(222);

    requisitionLineItemService.save(requisition, requisitionLineItem);

    assertEquals(expectedBeginningBalance, requisitionLineItem.getBeginningBalance());
  }

  @Test
  public void shouldNotInitiateBeginningBalanceWhenItIsNotDisplayed() {
    final Integer expectedBeginningBalance = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, false, false, true, true, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineItemService
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
            2, false, false, true, true, SourceType.USER_INPUT));
    requisitionTemplateColumnHashMap.put(TOTAL_QUANTITY_RECEIVED_FIELD,
        new RequisitionTemplateColumn(TOTAL_QUANTITY_RECEIVED_FIELD, TOTAL_QUANTITY_RECEIVED_FIELD,
            1, false, false, true, true, SourceType.USER_INPUT));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLineItemService.initiateRequisitionLineItemFields(requisition, requisitionTemplate);

    RequisitionTemplate requisitionTemplateList
        = requisitionTemplateService.searchRequisitionTemplates(requisition.getProgram()).get(0);

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
        requisitionLineItemService.searchRequisitionLineItems(
        requisition, null);

    assertEquals(1, receivedRequisitionLineItems.size());
    assertEquals(requisitionLineItem, receivedRequisitionLineItems.get(0));
  }

  private void generateInstances() {
    requisition = createTestRequisition(UUID.randomUUID(), period, program,
        RequisitionStatus.INITIATED);
    requisitionLineItem = createTestRequisitionLineItem(UUID.randomUUID(), 10, 20, requisition);

    requisition.setRequisitionLineItems(new ArrayList<>(Arrays.asList(requisitionLineItem)));
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
    return requisitionLineItem;
  }

  private void mockRepositories() {
    when(requisitionTemplateService
        .searchRequisitionTemplates(program))
        .thenReturn(Arrays.asList(requisitionTemplate));
    when(periodReferenceDataService
        .search(any(), any()))
        .thenReturn(Arrays.asList(periodDto));
    when(requisitionService
        .searchRequisitions(eq(requisition.getFacility()), eq(requisition.getProgram()),
            eq(null), eq(null), any(), eq(null), eq(null)))
        .thenReturn(Arrays.asList(requisition));
    when(requisitionLineItemRepository
        .searchRequisitionLineItems(eq(requisition), any()))
        .thenReturn(Arrays.asList(requisitionLineItem));
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
