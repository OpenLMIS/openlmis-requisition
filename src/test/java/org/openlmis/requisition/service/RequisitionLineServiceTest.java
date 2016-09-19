package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
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
public class RequisitionLineServiceTest {

  private static final String BEGINNING_BALANCE_FIELD = "beginningBalance";
  private static final String TOTAL_QUANTITY_RECEIVED_FIELD = "totalQuantityReceived";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  private Requisition requisition;
  private RequisitionLine requisitionLine;
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private RequisitionLineRepository requisitionLineRepository;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private RequisitionTemplateService requisitionTemplateService;


  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @InjectMocks
  private RequisitionLineService requisitionLineService;

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
  public void shouldInitiateRequisitionLineFieldsIfValidRequisitionProvided() {
    final Integer expectedBeginningBalance = 20;
    final Integer expectedTotalReceivedQuantity = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, false, true, true, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineService
        .initiateRequisitionLineFields(requisition);

    RequisitionLine requisitionLine = requisitionWithInitiatedLines
        .getRequisitionLines().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLine.getBeginningBalance());
    assertEquals(expectedTotalReceivedQuantity, requisitionLine.getTotalReceivedQuantity());
  }

  @Test
  public void shouldResetBeginningBalanceWhenSavingRequisitionLine() throws RequisitionException {
    final Integer expectedBeginningBalance = 20;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, true, true, false, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLine.setBeginningBalance(222);

    requisitionLineService.save(requisition, requisitionLine);

    assertEquals(expectedBeginningBalance, requisitionLine.getBeginningBalance());
  }

  @Test
  public void shouldNotInitiateBeginningBalanceWhenItIsNotDisplayed() {
    final Integer expectedBeginningBalance = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, false, false, true, true, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineService
        .initiateRequisitionLineFields(requisition);

    RequisitionLine requisitionLine = requisitionWithInitiatedLines
        .getRequisitionLines().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLine.getBeginningBalance());
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

    requisitionLineService.initiateRequisitionLineFields(requisition);

    List<RequisitionTemplate> requisitionTemplateList
        = requisitionTemplateService.searchRequisitionTemplates(requisition.getProgram());

    assertEquals(1, requisitionTemplateList.size());

    Map<String, RequisitionTemplateColumn> testRequisitionTemplateColumnHashMap
        = requisitionTemplateList.get(0).getColumnsMap();

    assertEquals(1, testRequisitionTemplateColumnHashMap
        .get(TOTAL_QUANTITY_RECEIVED_FIELD).getDisplayOrder());
    assertEquals(2, testRequisitionTemplateColumnHashMap
        .get(BEGINNING_BALANCE_FIELD).getDisplayOrder());
  }

  @Test
  public void shouldFindRequisitionLineIfItExists() {
    List<RequisitionLine> receivedRequisitionLines = requisitionLineService.searchRequisitionLines(
        requisition, null);

    assertEquals(1, receivedRequisitionLines.size());
    assertEquals(requisitionLine, receivedRequisitionLines.get(0));
  }

  private void generateInstances() {
    requisition = createTestRequisition(UUID.randomUUID(), period, program,
        RequisitionStatus.INITIATED);
    requisitionLine = createTestRequisitionLine(UUID.randomUUID(), 10, 20, requisition);

    requisition.setRequisitionLines(new ArrayList<>(Arrays.asList(requisitionLine)));
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

  private RequisitionLine createTestRequisitionLine(UUID product, Integer quantityRequested,
                                                    Integer stockInHand, Requisition requisition) {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setId(UUID.randomUUID());
    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(quantityRequested);
    requisitionLine.setStockInHand(stockInHand);
    requisitionLine.setRequisition(requisition);
    return requisitionLine;
  }

  private void mockRepositories() {
    when(requisitionTemplateService
        .searchRequisitionTemplates(program))
        .thenReturn(Arrays.asList(requisitionTemplate));
    when(periodReferenceDataService
        .search(any(), any()))
        .thenReturn(Arrays.asList(new ProcessingPeriodDto()));
    when(requisitionService
        .searchRequisitions(eq(requisition.getFacility()), eq(requisition.getProgram()),
            eq(null), eq(null), any(), eq(null), eq(null)))
        .thenReturn(Arrays.asList(requisition));
    when(requisitionLineRepository
        .searchRequisitionLines(eq(requisition), any()))
        .thenReturn(Arrays.asList(requisitionLine));
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(new ProgramDto());
    when(periodReferenceDataService
        .findOne(any()))
        .thenReturn(new ProcessingPeriodDto());
  }
}
