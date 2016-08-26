package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.service.ConfigurationSettingService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class RequisitionServiceTest {

  private static final String REQUISITION_TEST_NAME = "RequisitionServiceTest";

  private Requisition requisition;

  private Requisition requisition2;
  private Requisition requisition3;
  private Set<Requisition> requisitions;
  private List<RequisitionLine> requisitionLines;
  private SupervisoryNode supervisoryNode;
  private User user;
  private Facility facility;
  private ProcessingPeriod period;
  private Program program;

  private Product product;

  @Mock
  private UserRepository userRepository;

  @Mock
  private RequisitionLineService requisitionLineService;

  @Mock
  private RequisitionLineRepository requisitionLineRepository;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private RequisitionRepository requisitionRepository;

  @InjectMocks
  private RequisitionService requisitionService;

  @Before
  public void setUp() {
    generateInstances();
    initMocks(this);
    mockRepositories();
  }

  @Test
  public void shouldDeleteRequisitionIfItIsInitiated() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.INITIATED);
    boolean deleted = requisitionService.tryDelete(requisition.getId());

    assertTrue(deleted);
  }

  @Test
  public void shouldNotDeleteRequisitionWhenStatusIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    boolean deleted = requisitionService.tryDelete(requisition.getId());

    assertFalse(deleted);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenDeletingNotExistingRequisition()
          throws RequisitionException {
    UUID deletedRequisitionId = requisition.getId();
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    requisitionService.tryDelete(deletedRequisitionId);
  }

  @Test
  public void shouldSkipRequisitionIfItIsValid() throws RequisitionException {
    Requisition skippedRequisition = requisitionService.skip(requisition.getId());

    assertEquals(skippedRequisition.getStatus(), RequisitionStatus.SKIPPED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenSkippingNotSkippableProgram()
          throws RequisitionException {
    requisition.getProgram().setPeriodsSkippable(false);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenSkippingNotExistingRequisition()
          throws RequisitionException {
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    requisitionService.skip(requisition.getId());
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsAuthorized()
          throws RequisitionException {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenRejectingRequisitionWithStatusApproved()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.APPROVED);
    requisitionService.reject(requisition.getId());
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenRejectingNotExistingRequisition()
          throws RequisitionException {
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);
    requisitionService.reject(requisition.getId());
  }

  @Test
  public void shouldGetAuthorizedRequisitionsIfSupervisoryNodeProvided() {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setSupervisoryNode(supervisoryNode);
    requisition2.setStatus(RequisitionStatus.AUTHORIZED);
    requisition2.setSupervisoryNode(supervisoryNode);
    requisition3.setSupervisoryNode(supervisoryNode);

    when(requisitionRepository.searchRequisitions(
        null, null, null, null, null, supervisoryNode, null))
        .thenReturn(Arrays.asList(requisition, requisition2));

    List<Requisition> authorizedRequisitions =
        requisitionService.getAuthorizedRequisitions(supervisoryNode);
    List<Requisition> expected = Arrays.asList(requisition, requisition2);

    assertEquals(expected, authorizedRequisitions);
  }

  @Test
  public void shouldGetRequisitionsForApprovalIfUserHasSupervisedNode() {
    user.setSupervisedNode(supervisoryNode);
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setSupervisoryNode(supervisoryNode);
    requisition2.setSupervisoryNode(supervisoryNode);

    when(userRepository
            .findOne(user.getId()))
            .thenReturn(user);
    when(requisitionRepository
            .searchRequisitions(null, null, null, null, null, supervisoryNode, null))
            .thenReturn(Arrays.asList(requisition));

    List<Requisition> requisitionsForApproval = requisitionService
            .getRequisitionsForApproval(user.getId());
    List<Requisition> expected = Arrays.asList(requisition);

    assertEquals(expected, requisitionsForApproval);
  }

  @Test
  public void shouldInitiateRequisitionIfItNotAlreadyExist() throws RequisitionException {
    requisition.setStatus(null);
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    Requisition initiatedRequisition = requisitionService.initiateRequisition(requisition);

    assertEquals(initiatedRequisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenInitiatingEmptyRequisition()
          throws RequisitionException {
    requisitionService.initiateRequisition(null);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenInitiatingAlreadyExistingRequisition()
          throws RequisitionException {
    requisitionService.initiateRequisition(requisition);
  }

  @Test
  public void shouldAuthorizeRequisitionIfItIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionService.authorize(requisition.getId(), requisition, false);

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionIfAuthorizationIsConfiguredToBeSkipped()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    when(configurationSettingService
            .getBoolValue("skipAuthorization"))
            .thenReturn(true);
    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingInitiatedRequisition()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingNotExistingRequisition()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() {
    List<Requisition> requisitions = Arrays.asList(requisition);
    List<Requisition> expectedRequisitions = requisitionService
        .releaseRequisitionsAsOrder(requisitions);
    assertEquals(RequisitionStatus.RELEASED, expectedRequisitions.get(0).getStatus());
  }

  @Test
  public void shouldFindRequisitionIfItExists() {
    when(requisitionRepository.searchRequisitions(requisition.getFacility(),
        requisition.getProgram(),
        requisition.getCreatedDate().minusDays(2),
        requisition.getCreatedDate().plusDays(2),
        requisition.getProcessingPeriod(),
        requisition.getSupervisoryNode(),
        requisition.getStatus())).thenReturn(Arrays.asList(requisition));

    List<Requisition> receivedRequisitions = requisitionService.searchRequisitions(
        requisition.getFacility(),
        requisition.getProgram(),
        requisition.getCreatedDate().minusDays(2),
        requisition.getCreatedDate().plusDays(2),
        requisition.getProcessingPeriod(),
        requisition.getSupervisoryNode(),
        requisition.getStatus());

    assertEquals(1,receivedRequisitions.size());
    assertEquals(
        receivedRequisitions.get(0).getFacility().getId(),
        requisition.getFacility().getId());
    assertEquals(
        receivedRequisitions.get(0).getProgram().getId(),
        requisition.getProgram().getId());
    assertTrue(
        receivedRequisitions.get(0).getCreatedDate().isAfter(
            requisition.getCreatedDate().minusDays(2)));
    assertTrue(
        receivedRequisitions.get(0).getCreatedDate().isBefore(
            requisition.getCreatedDate().plusDays(2)));
    assertEquals(
        receivedRequisitions.get(0).getProcessingPeriod().getId(),
        requisition.getProcessingPeriod().getId());
    assertEquals(
        receivedRequisitions.get(0).getSupervisoryNode().getId(),
        requisition.getSupervisoryNode().getId());
    assertEquals(
        receivedRequisitions.get(0).getStatus(),
        requisition.getStatus());
  }

  private void generateInstances() {
    user = generateUser();
    program = generateProgram();
    facility = generateFacility();
    period = generatePeriod();
    supervisoryNode = generateSupervisoryNode();
    requisitions = generateRequisitions();
    product = generateProduct();
    requisitionLines =  generateRequisitionLines();
    requisition.setRequisitionLines(requisitionLines);
  }

  private User generateUser() {
    user = new User();
    user.setId(UUID.randomUUID());
    user.setUsername("Username");
    user.setFirstName("Firstname");
    user.setLastName("Lastname");
    return user;
  }

  private Program generateProgram() {
    program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode(REQUISITION_TEST_NAME);
    program.setPeriodsSkippable(true);
    return program;
  }

  private Facility generateFacility() {
    FacilityType facilityType = new FacilityType();
    facilityType.setId(UUID.randomUUID());
    facilityType.setCode(REQUISITION_TEST_NAME);

    GeographicLevel level = new GeographicLevel();
    level.setId(UUID.randomUUID());
    level.setCode(REQUISITION_TEST_NAME);
    level.setLevelNumber(1);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setId(UUID.randomUUID());
    geographicZone.setCode(REQUISITION_TEST_NAME);
    geographicZone.setLevel(level);

    facility = new Facility();
    facility.setId(UUID.randomUUID());
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(REQUISITION_TEST_NAME);
    facility.setActive(true);
    facility.setEnabled(true);
    return facility;
  }

  private ProcessingPeriod generatePeriod() {
    ProcessingSchedule schedule = new ProcessingSchedule();
    schedule.setName("scheduleName");
    schedule.setCode(REQUISITION_TEST_NAME);

    period = new ProcessingPeriod();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(schedule);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 1, 2));
    period.setName("periodName");
    period.setDescription("description");
    return period;
  }

  private SupervisoryNode generateSupervisoryNode() {
    supervisoryNode = new SupervisoryNode();
    supervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setCode("Test");
    supervisoryNode.setFacility(facility);
    return supervisoryNode;
  }

  private Set<Requisition> generateRequisitions() {
    requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(supervisoryNode);

    requisition2 = new Requisition();
    requisition2.setId(UUID.randomUUID());
    requisition2.setFacility(facility);
    requisition2.setProcessingPeriod(period);
    requisition2.setProgram(program);
    requisition2.setCreatedDate(LocalDateTime.now().minusDays(5));
    requisition2.setStatus(RequisitionStatus.INITIATED);
    requisition2.setSupervisoryNode(supervisoryNode);

    requisition3 = new Requisition();
    requisition3.setId(UUID.randomUUID());
    requisition3.setFacility(facility);
    requisition3.setProcessingPeriod(period);
    requisition3.setProgram(program);
    requisition.setCreatedDate(LocalDateTime.now().minusDays(9));
    requisition3.setStatus(RequisitionStatus.INITIATED);
    requisition3.setSupervisoryNode(supervisoryNode);

    requisitions = new HashSet<>();
    requisitions.add(requisition);
    requisitions.add(requisition2);
    requisitions.add(requisition3);
    return requisitions;
  }

  private Product generateProduct() {
    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);

    product = new Product();
    product.setCode("code");
    product.setPrimaryName("product");
    product.setDispensingUnit("unit");
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory1);
    return product;
  }

  private List<RequisitionLine> generateRequisitionLines() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequisition(requisition);
    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);

    requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);
    return requisitionLines;
  }

  private void mockRepositories() {
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(requisition);
    when(requisitionRepository
            .save(requisition))
            .thenReturn(requisition);
  }
}
