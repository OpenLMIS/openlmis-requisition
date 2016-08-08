package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@SuppressWarnings("PMD.TooManyMethods")
@Transactional
public class RequisitionServiceTest {
  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private UserRepository userRepository;

  private Requisition requisition;
  private Requisition requisition2;
  private Requisition requisition3;
  private SupervisoryNode supervisoryNode;
  private User user;
  private Facility facility;
  private Period period;
  private Program program;


  @Before
  public void setUp() {
    requisitionRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    programRepository.deleteAll();

    createTestRequisition();
  }

  @After
  public void cleanUp() {
    requisitionRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    programRepository.deleteAll();
  }

  @Test
  public void testTryDelete() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    boolean deleted = requisitionService.tryDelete(requisition.getId());
    Assert.assertTrue(deleted);
  }

  @Test
  public void testTryDeleteBadStatus() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    boolean deleted = requisitionService.tryDelete(requisition.getId());
    Assert.assertFalse(deleted);
  }

  @Test(expected = RequisitionException.class)
  public void testTryDeleteRequisitionDoesNotExist() throws RequisitionException {
    UUID id = requisition.getId();
    requisitionRepository.delete(id);

    boolean deleted = requisitionService.tryDelete(id);
    Assert.assertFalse(deleted);
  }

  @Test
  public void shouldSkipRequisition() throws RequisitionException {

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

    Requisition skipResult = requisitionService.skip(requisition.getId());

    Assert.assertNotNull(skipResult);
    Assert.assertEquals(skipResult.getStatus(), RequisitionStatus.SKIPPED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotSkipRequisitionIfProgramIsNotSkippable() throws RequisitionException {

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

    requisition.getProgram().setPeriodsSkippable(false);
    requisitionService.skip(requisition.getId());
  }

  @Test
  public void shouldRejectRequisition() throws RequisitionException {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    requisitionService.reject(requisition.getId());

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotAllowRejectionIfRequisitionStatusIsWrong() throws RequisitionException {

    requisition.setStatus(RequisitionStatus.APPROVED);
    requisitionRepository.save(requisition);

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.APPROVED);

    requisitionService.reject(requisition.getId());
  }

  @Test
  @Transactional
  public void getAuthorizedRequisitionsForSupervisorNode() {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition);

    requisition2.setStatus(RequisitionStatus.AUTHORIZED);
    requisition2.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition2);

    requisition3.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition3);

    List<Requisition> requisitionList =
        requisitionService.getAuthorizedRequisitions(supervisoryNode);
    List<Requisition> expected = new ArrayList<>();
    expected.add(requisition);
    expected.add(requisition2);

    Assert.assertEquals(expected, requisitionList);
  }

  @Test
  @Transactional
  public void getRequisitionsForApprove() {
    user.setSupervisedNode(supervisoryNode);
    user = userRepository.save(user);

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition);

    requisition2.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition2);

    List<Requisition> requisitionList = requisitionService.getRequisitionsForApproval(user.getId());
    List<Requisition> expected = new ArrayList<>();
    expected.add(requisition);

    Assert.assertEquals(expected, requisitionList);
  }

  @Test
  public void shouldAuthorizeRequisition() throws RequisitionException {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    
    requisitionService.authorize(requisition.getId(), requisition, false);

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotInitiateRequisitionWhenItAlreadyExists() throws RequisitionException {
    requisitionService.initiateRequisition(requisition);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotAllowAuthorizationIfRequisitionStatusIsWrong() throws RequisitionException {

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() {
    Assert.assertNotEquals(RequisitionStatus.RELEASED, requisition.getStatus());
    List<Requisition> requisitions = Collections.singletonList(requisition);
    requisitionService.releaseRequisitionsAsOrder(requisitions);

    requisition = requisitionRepository.findOne(requisition.getId());
    Assert.assertEquals(RequisitionStatus.RELEASED, requisition.getStatus());
  }

  @Test
  public void testSearchRequisitions() {
    List<Requisition> receivedRequisitions = requisitionService.searchRequisitions(
            requisition.getFacility(),
            requisition.getProgram(),
            requisition.getCreatedDate().minusDays(2),
            requisition.getCreatedDate().plusDays(2),
            requisition.getProcessingPeriod(),
            requisition.getSupervisoryNode(),
            requisition.getStatus());

    Assert.assertEquals(1,receivedRequisitions.size());
    for ( Requisition receivedRequisition : receivedRequisitions ) {
      Assert.assertEquals(
              receivedRequisition.getFacility().getId(),
              requisition.getFacility().getId());
      Assert.assertEquals(
              receivedRequisition.getProgram().getId(),
              requisition.getProgram().getId());
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isAfter(
                      requisition.getCreatedDate().minusDays(2)));
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isBefore(
                      requisition.getCreatedDate().plusDays(2)));
      Assert.assertEquals(
              receivedRequisition.getProcessingPeriod().getId(),
              requisition.getProcessingPeriod().getId());
      Assert.assertEquals(
              receivedRequisition.getSupervisoryNode().getId(),
              requisition.getSupervisoryNode().getId());
      Assert.assertEquals(
              receivedRequisition.getStatus(),
              requisition.getStatus());
    }
  }

  private void createTestRequisition() {
    user = new User();
    user.setUsername("Username");
    user.setFirstName("Firstname");
    user.setLastName("Lastname");
    user = userRepository.save(user);

    program = new Program();
    program.setCode(REQUISITION_REPOSITORY_NAME);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    Program program2 = new Program();
    program2.setCode("test code");
    program2.setPeriodsSkippable(true);
    programRepository.save(program2);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(REQUISITION_REPOSITORY_NAME);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(REQUISITION_REPOSITORY_NAME);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(REQUISITION_REPOSITORY_NAME);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(REQUISITION_REPOSITORY_NAME);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setName("scheduleName");
    schedule.setCode(REQUISITION_REPOSITORY_NAME);
    scheduleRepository.save(schedule);

    period = new Period();
    period.setProcessingSchedule(schedule);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 1, 2));
    period.setName("periodName");
    period.setDescription("description");
    periodRepository.save(period);

    supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("Test");
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);

    requisition = new Requisition();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition);

    requisition2 = new Requisition();
    requisition2.setFacility(facility);
    requisition2.setProcessingPeriod(period);
    requisition2.setProgram(program2);
    requisition2.setCreatedDate(LocalDateTime.now().minusDays(5));
    requisition2.setStatus(RequisitionStatus.INITIATED);
    requisition2.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition2);

    requisition3 = new Requisition();
    requisition3.setFacility(facility);
    requisition3.setProcessingPeriod(period);
    requisition3.setProgram(program2);
    requisition.setCreatedDate(LocalDateTime.now().minusDays(9));
    requisition3.setStatus(RequisitionStatus.INITIATED);
    requisition3.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition3);
  }
}
