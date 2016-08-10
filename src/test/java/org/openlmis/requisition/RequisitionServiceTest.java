package org.openlmis.requisition;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@Transactional
public class RequisitionServiceTest {

  private static final String REQUISITION_TEST_NAME = "RequisitionServiceTest";

  private Requisition requisition;
  private Requisition requisition2;
  private Requisition requisition3;
  private SupervisoryNode supervisoryNode;
  private User user;
  private Facility facility;
  private Period period;
  private Program program;
  private RequisitionRepository requisitionRepository;
  private RequisitionService requisitionService;

  @Before
  public void setUp() {
    requisitionService = new RequisitionService();
    generateInstances();
    mockRepositories();
    initMocks(this);
  }

  @Test
  public void shouldRejectRequisition() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());
    Assert.assertEquals(returnedRequisition.getStatus(), RequisitionStatus.INITIATED);
  }

  private void generateInstances() {
    user = new User();
    user.setId(UUID.randomUUID());
    user.setUsername("Username");
    user.setFirstName("Firstname");
    user.setLastName("Lastname");

    program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode(REQUISITION_TEST_NAME);
    program.setPeriodsSkippable(true);

    Program program2 = new Program();
    program2.setId(UUID.randomUUID());
    program2.setCode("code");
    program2.setPeriodsSkippable(true);

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

    Schedule schedule = new Schedule();
    schedule.setName("scheduleName");
    schedule.setCode(REQUISITION_TEST_NAME);

    period = new Period();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(schedule);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 1, 2));
    period.setName("periodName");
    period.setDescription("description");

    supervisoryNode = new SupervisoryNode();
    supervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setCode("Test");
    supervisoryNode.setFacility(facility);

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
    requisition2.setProgram(program2);
    requisition2.setCreatedDate(LocalDateTime.now().minusDays(5));
    requisition2.setStatus(RequisitionStatus.INITIATED);
    requisition2.setSupervisoryNode(supervisoryNode);

    requisition3 = new Requisition();
    requisition3.setId(UUID.randomUUID());
    requisition3.setFacility(facility);
    requisition3.setProcessingPeriod(period);
    requisition3.setProgram(program2);
    requisition.setCreatedDate(LocalDateTime.now().minusDays(9));
    requisition3.setStatus(RequisitionStatus.INITIATED);
    requisition3.setSupervisoryNode(supervisoryNode);
  }

  private void mockRepositories() {
    requisitionRepository = mock(RequisitionRepository.class);
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);
    when(requisitionRepository.findOne(requisition2.getId())).thenReturn(requisition2);
    when(requisitionRepository.findOne(requisition3.getId())).thenReturn(requisition3);
    when(requisitionRepository.save(requisition)).thenReturn(requisition);
    when(requisitionRepository.save(requisition2)).thenReturn(requisition2);
    when(requisitionRepository.save(requisition3)).thenReturn(requisition3);
    ReflectionTestUtils.setField(requisitionService, "requisitionRepository",
        requisitionRepository, RequisitionRepository.class);

    //mock other repositories with needed information
  }
}
