package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.service.PeriodService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class RequisitionLineServiceTest {

  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionLineServiceIntegrationTest";
  private static final String BEGINNING_BALANCE_FIELD = "beginningBalance";
  private static final String TOTAL_QUANTITY_RECEIVED_FIELD = "totalQuantityReceived";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  private Requisition requisition;
  private Requisition requisition2;
  private RequisitionLine requisitionLine;
  private RequisitionLine requisitionLine2;
  private RequisitionTemplate requisitionTemplate;
  private Program program;
  private Product product;
  private Period period;
  private Period period2;

  @Mock
  private RequisitionLineRepository requisitionLineRepository;

  @Mock
  private PeriodService periodService;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private RequisitionTemplateService requisitionTemplateService;

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @InjectMocks
  private RequisitionLineService requisitionLineService;

  @Before
  public void setUp() {
    generateInstances();
    initMocks(this);
    mockRepositories();
  }

  @Test
  public void testShouldInitiateRequisitionLineFieldsIfValidRequisitionProvided() {
    final Integer expectedBeginningBalance = 20;
    final Integer expectedTotalReceivedQuantity = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, false, true, true, SOURCE));

    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplate.setProgram(requisition2.getProgram());

    Requisition requisitionWithInitiatedLines = requisitionLineService
        .initiateRequisitionLineFields(requisition2);

    RequisitionLine requisitionLine = requisitionWithInitiatedLines
        .getRequisitionLines().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLine.getBeginningBalance());
    assertEquals(expectedTotalReceivedQuantity, requisitionLine.getTotalReceivedQuantity());
  }


  @Test
  public void testShouldResetBeginningBalanceWhenSavingRequisitionLine()
      throws RequisitionException {
    final Integer expectedBeginningBalance = 20;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, true, true, true, false, SOURCE));

    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLine2.setBeginningBalance(222);

    requisitionLineService.save(requisition, requisitionLine2);

    assertEquals(expectedBeginningBalance, requisitionLine2.getBeginningBalance());
  }


  @Test
  public void testShouldNotInitiateBeginningBalanceWhenItIsNotDisplayed() {
    final Integer expectedBeginningBalance = 0;

    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD, new RequisitionTemplateColumn(
        BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1, false, false, true, true, SOURCE));

    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    Requisition requisitionWithInitiatedLines = requisitionLineService
        .initiateRequisitionLineFields(requisition2);

    RequisitionLine requisitionLine = requisitionWithInitiatedLines
        .getRequisitionLines().iterator().next();

    assertEquals(expectedBeginningBalance, requisitionLine.getBeginningBalance());
  }


  @Test
  public void testShouldDisplayColumnsInCorrectOrder() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();

    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
        new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD,
            2, false, false, true, true, SourceType.USER_INPUT));
    requisitionTemplateColumnHashMap.put(TOTAL_QUANTITY_RECEIVED_FIELD,
        new RequisitionTemplateColumn(TOTAL_QUANTITY_RECEIVED_FIELD, TOTAL_QUANTITY_RECEIVED_FIELD,
            1, false, false, true, true, SourceType.USER_INPUT));

    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);

    requisitionLineService.initiateRequisitionLineFields(requisition2);

    List<RequisitionTemplate> requisitionTemplateList
        = requisitionTemplateService.searchRequisitionTemplates(requisition2.getProgram());

    assertEquals(1 ,requisitionTemplateList.size());

    Map<String, RequisitionTemplateColumn> testRequisitionTemplateColumnHashMap
        = requisitionTemplateList.get(0).getColumnsMap();

    assertEquals(1, testRequisitionTemplateColumnHashMap
        .get(TOTAL_QUANTITY_RECEIVED_FIELD).getDisplayOrder());
    assertEquals(2, testRequisitionTemplateColumnHashMap
        .get(BEGINNING_BALANCE_FIELD).getDisplayOrder());
  }

  @Test
  public void testShouldFindRequisitionLineIfItExists() {
    when(requisitionLineRepository.searchRequisitionLines(
        requisition, null)).thenReturn(Arrays.asList(requisitionLine));

    List<RequisitionLine> receivedRequisitionLines = requisitionLineService.searchRequisitionLines(
        requisition, null);

    assertEquals(1, receivedRequisitionLines.size());

    RequisitionLine receivedRequisitionLine = receivedRequisitionLines.get(0);

    assertEquals(requisitionLine, receivedRequisitionLine);
  }

  private void generateInstances() {
    ProductCategory productCategory = new ProductCategory("code", "name", 1);

    product = new Product();
    product.setId(UUID.randomUUID());
    product.setProductCategory(productCategory);
    product.setCode(REQUISITION_REPOSITORY_NAME);
    product.setPrimaryName(REQUISITION_REPOSITORY_NAME);
    product.setDispensingUnit(REQUISITION_REPOSITORY_NAME);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);

    program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode(REQUISITION_REPOSITORY_NAME);
    program.setPeriodsSkippable(true);

    FacilityType facilityType = new FacilityType();
    facilityType.setId(UUID.randomUUID());
    facilityType.setCode(REQUISITION_REPOSITORY_NAME);

    GeographicLevel level = new GeographicLevel();
    level.setId(UUID.randomUUID());
    level.setCode(REQUISITION_REPOSITORY_NAME);
    level.setLevelNumber(1);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setId(UUID.randomUUID());
    geographicZone.setCode(REQUISITION_REPOSITORY_NAME);
    geographicZone.setLevel(level);

    Facility facility = new Facility();
    facility.setId(UUID.randomUUID());
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(REQUISITION_REPOSITORY_NAME);
    facility.setActive(true);
    facility.setEnabled(true);

    Schedule schedule = new Schedule();
    schedule.setId(UUID.randomUUID());
    schedule.setCode("code");
    schedule.setName("name");

    period = createTestPeriod("description", "name1", schedule,
        LocalDate.of(2016, 1, 1), LocalDate.of(2016, 2, 1));
    period2 = createTestPeriod("description", "name2", schedule,
        LocalDate.of(2016, 2, 1), LocalDate.of(2016, 3, 1));

    requisition = createTestRequisition(facility, period, program,
        RequisitionStatus.INITIATED);
    requisition2 = createTestRequisition(facility, period2, program,
        RequisitionStatus.INITIATED);

    requisitionLine = createTestRequisitionLine(product, 10, 20, requisition);
    requisitionLine2 = createTestRequisitionLine(
        product, 100, 50, requisition2);

    requisition.setRequisitionLines(new HashSet<>(Arrays.asList(requisitionLine)));
    requisition2.setRequisitionLines(new HashSet<>(Arrays.asList(requisitionLine2)));
    requisitionTemplate = new RequisitionTemplate();

  }

  private Requisition createTestRequisition(Facility facility, Period period, Program program,
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

  private Period createTestPeriod(String description, String name,
                                  Schedule schedule, LocalDate startDate, LocalDate endDate) {
    Period period = new Period();
    period.setId(UUID.randomUUID());
    period.setDescription(description);
    period.setName(name);
    period.setProcessingSchedule(schedule);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return period;
  }

  private void mockRepositories() {
    when(requisitionTemplateService
            .searchRequisitionTemplates(requisition2.getProgram()))
            .thenReturn(Arrays.asList(requisitionTemplate));
    when(periodService
            .searchPeriods(requisition2.getProcessingPeriod().getProcessingSchedule(),
                    requisition2.getProcessingPeriod().getStartDate()))
            .thenReturn(Arrays.asList(period));
    when(requisitionService
            .searchRequisitions(requisition.getFacility(), requisition.getProgram(),
                    null,null, period, null, null))
            .thenReturn(Arrays.asList(requisition));
    when(requisitionLineService
            .searchRequisitionLines(requisition, product))
            .thenReturn(Arrays.asList(requisitionLine));
  }
}
