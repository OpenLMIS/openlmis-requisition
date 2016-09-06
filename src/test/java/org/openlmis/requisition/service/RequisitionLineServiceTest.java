package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.service.ProcessingPeriodService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
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
  private ProcessingPeriodService periodService;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private RequisitionTemplateService requisitionTemplateService;

  @Mock
  private Program program;

  @Mock
  private ProcessingPeriod period;

  @InjectMocks
  private RequisitionLineService requisitionLineService;

  @Before
  public void setUp() {
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
  public void shouldCalculateRequisitionLineFieldsCorrectly() throws RequisitionException {
    final Integer expectedStockOnHand = 1200;

    requisitionLine.setTotalLossesAndAdjustments(-100);
    requisitionLine.setTotalConsumedQuantity(200);
    requisitionLine.setTotalReceivedQuantity(500);
    requisitionLine.setBeginningBalance(1000);

    requisitionLineService.calculateRequisitionLineFields(requisition);

    assertEquals(expectedStockOnHand, requisitionLine.getStockOnHand());
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
    requisition = createTestRequisition(mock(Facility.class), period, program,
        RequisitionStatus.INITIATED);
    requisitionLine = createTestRequisitionLine(mock(Product.class), 10, 20, requisition);

    requisition.setRequisitionLines(new ArrayList<>(Arrays.asList(requisitionLine)));
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
  }

  private Requisition createTestRequisition(Facility facility, ProcessingPeriod period,
                                            Program program,
                                            RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(requisitionStatus);
    return requisition;
  }

  private RequisitionLine createTestRequisitionLine(Product product, Integer quantityRequested,
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
    when(periodService
        .searchPeriods(any(), any()))
        .thenReturn(Arrays.asList(period));
    when(requisitionService
        .searchRequisitions(requisition.getFacility(), requisition.getProgram(),
            null,null, period, null, null))
        .thenReturn(Arrays.asList(requisition));
    when(requisitionLineRepository
        .searchRequisitionLines(eq(requisition), any()))
        .thenReturn(Arrays.asList(requisitionLine));
  }
}
