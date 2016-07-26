package org.openlmis.referencedata.service;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.RightRepository;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
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
import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class RequisitionServiceTest {
  private static final String requisitionRepositoryName = "RequisitionRepositoryIntegrationTest";

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

  @Autowired
  private RoleRepository roleRepository;

  @Autowired
  private RightRepository rightRepository;

  private Requisition requisition;
  private Requisition requisition2;
  private Requisition requisition3;
  private SupervisoryNode supervisoryNode;
  private User user;

  /** Prepare the test environment. */
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

  @Test
  public void testTryDelete() {
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    boolean deleted = requisitionService.tryDelete(requisition.getId());
    Assert.assertTrue(deleted);
  }

  @Test
  public void testTryDeleteBadStatus() {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    boolean deleted = requisitionService.tryDelete(requisition.getId());
    Assert.assertFalse(deleted);
  }

  @Test
  public void shouldSkipRequisition() {

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

    boolean skipResult = requisitionService.skip(requisition.getId());

    Assert.assertTrue(skipResult);
    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.SKIPPED);
  }

  @Test
  public void shouldNotSkipRequisitionIfProgramIsNotSkippable() {

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

    requisition.getProgram().setPeriodsSkippable(false);
    boolean skipResult = requisitionService.skip(requisition.getId());

    Assert.assertFalse(skipResult);
    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

  }

  @Test
  public void shouldRejectRequisition() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    requisitionService.reject(requisition.getId());

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotAllowRejectionIfRequisitionStatusIsWrong() {

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
    Right approveRight = new Right();
    approveRight.setName("APPROVE_REQUISITION");
    approveRight.setRightType("APPROVE_REQUISITION");
    rightRepository.save(approveRight);
    List<Right> rightList = new ArrayList<>();
    rightList.add(approveRight);

    Role approveRole = new Role();
    approveRole.setRights(rightList);
    approveRole.setSupervisedNode(supervisoryNode);
    approveRole.setName("Approve Role");
    roleRepository.save(approveRole);
    List<Role> roleList = new ArrayList<>();
    roleList.add(approveRole);

    user.setRoles(roleList);
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

 /* @Test
  @Transactional
  public void getCommentsByReqIdTest() {
    Comment comment = new Comment();
    comment.setAuthor(user);
    comment.setRequisition(requisition);
    comment.setCommentText("First comment");
    commentRepository.save(comment);

    Comment comment1 = new Comment();
    comment1.setAuthor(user);
    comment1.setRequisition(requisition);
    comment1.setCommentText("Second comment");
    commentRepository.save(comment1);

    requisitionRepository.save(requisition);

    List<Comment> comments = requisitionService.getCommentsByReqId(requisition.getId());
    List<Comment> expected = new ArrayList<>();
    expected.add(comment);
    expected.add(comment1);

    Assert.assertEquals(expected, comments);
  }*/

  @Test
  public void shouldAuthorizeRequisition() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    
    requisitionService.authorize(requisition.getId());

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotAllowAuthorizationIfRequisitionStatusIsWrong() {

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    requisitionService.authorize(requisition.getId());
  }

  private void createTestRequisition() {
    user = new User();
    user.setUsername("Username");
    user.setFirstName("Firstname");
    user.setLastName("Lastname");
    user = userRepository.save(user);

    Program program = new Program();
    program.setCode(requisitionRepositoryName);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionRepositoryName);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionRepositoryName);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionRepositoryName);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionRepositoryName);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setCode(requisitionRepositoryName);
    schedule.setName("Test");
    scheduleRepository.save(schedule);

    Period period = new Period();
    period.setProcessingSchedule(schedule);
    period.setEndDate(LocalDate.of(2016, 2, 2));
    period.setStartDate(LocalDate.of(2016, 2, 1));
    period.setName("Test name");
    periodRepository.save(period);

    supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("Test");
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);

    requisition = new Requisition();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    requisition2 = new Requisition();
    requisition2.setFacility(facility);
    requisition2.setProcessingPeriod(period);
    requisition2.setProgram(program);
    requisition2.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition2);

    requisition3 = new Requisition();
    requisition3.setFacility(facility);
    requisition3.setProcessingPeriod(period);
    requisition3.setProgram(program);
    requisition3.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition3);
  }
}
