package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  @Autowired
  private RequisitionRepository repository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  private List<Requisition> requisitions;

  RequisitionRepository getRepository() {
    return this.repository;
  }

  Requisition generateInstance() {
    Requisition requisition = new Requisition();
    requisition.setProgram(generateProgram());
    requisition.setFacility(generateFacility());
    requisition.setProcessingPeriod(generatePeriod());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setCreatedDate(LocalDateTime.now().plusDays(requisitions.size()));
    requisition.setSupervisoryNode(generateSupervisoryNode());
    return requisition;
  }

  @Before
  public void setUp() {
    requisitions = new ArrayList<>();
    for (int requisitionLinesCount = 0; requisitionLinesCount < 5; requisitionLinesCount++) {
      requisitions.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchRequisitionsByAllParameters() {
    Requisition requisition = new Requisition();
    requisition.setFacility(requisitions.get(0).getFacility());
    requisition.setProgram(requisitions.get(0).getProgram());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setProcessingPeriod(requisitions.get(0).getProcessingPeriod());
    requisition.setSupervisoryNode(requisitions.get(0).getSupervisoryNode());
    requisition.setStatus(requisitions.get(0).getStatus());
    repository.save(requisition);
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            requisitions.get(0).getFacility(),
            requisitions.get(0).getProgram(),
            requisitions.get(0).getCreatedDate().minusDays(1),
            requisitions.get(0).getCreatedDate().plusDays(2),
            requisitions.get(0).getProcessingPeriod(),
            requisitions.get(0).getSupervisoryNode(),
            requisitions.get(0).getStatus());

    Assert.assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      Assert.assertEquals(
              receivedRequisition.getProgram().getId(),
              requisitions.get(0).getProgram().getId());
      Assert.assertEquals(
              receivedRequisition.getProcessingPeriod().getId(),
              requisitions.get(0).getProcessingPeriod().getId());
      Assert.assertEquals(
              receivedRequisition.getFacility().getId(),
              requisitions.get(0).getFacility().getId());
      Assert.assertEquals(
              receivedRequisition.getSupervisoryNode().getId(),
              requisitions.get(0).getSupervisoryNode().getId());
      Assert.assertEquals(
              receivedRequisition.getStatus(),
              requisitions.get(0).getStatus());
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isBefore(
                      requisitions.get(0).getCreatedDate().plusDays(2)));
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isAfter(
                      requisitions.get(0).getCreatedDate().minusDays(1)));
    }
  }

  @Test
  public void testSearchRequisitionsByFacilityAndProgram() {
    Requisition requisition = new Requisition();
    requisition.setFacility(requisitions.get(0).getFacility());
    requisition.setProgram(requisitions.get(0).getProgram());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setProcessingPeriod(requisitions.get(0).getProcessingPeriod());
    requisition.setSupervisoryNode(requisitions.get(0).getSupervisoryNode());
    requisition.setStatus(requisitions.get(0).getStatus());
    repository.save(requisition);
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            requisitions.get(0).getFacility(),
            requisitions.get(0).getProgram(),
            null,
            null,
            null,
            null,
            null);

    Assert.assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      Assert.assertEquals(
              receivedRequisition.getProgram().getId(),
              requisitions.get(0).getProgram().getId());
      Assert.assertEquals(
              receivedRequisition.getFacility().getId(),
              requisitions.get(0).getFacility().getId());
    }
  }

  @Test
  public void testSearchRequisitionsByAllParametersNull() {
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            null, null, null, null, null, null, null);

    Assert.assertEquals(5, receivedRequisitions.size());
  }

  private SupervisoryNode generateSupervisoryNode() {
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("code" + this.getNextInstanceNumber());
    supervisoryNode.setFacility(generateFacility());
    supervisoryNodeRepository.save(supervisoryNode);
    return supervisoryNode;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("Program" + this.getNextInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }

  private Period generatePeriod() {
    Period period = new Period();
    Integer instanceNumber = this.getNextInstanceNumber();
    period.setName("PeriodName" + instanceNumber);
    period.setDescription("PeriodDescription" + instanceNumber);
    period.setEndDate(LocalDate.now().plusDays(instanceNumber));
    period.setStartDate(LocalDate.now().minusDays(instanceNumber));
    period.setProcessingSchedule(generateSchedule());
    periodRepository.save(period);
    return period;
  }

  private Schedule generateSchedule() {
    Schedule schedule = new Schedule();
    schedule.setCode("Schedule" + this.getNextInstanceNumber());
    schedule.setName("name" + this.getNextInstanceNumber());
    scheduleRepository.save(schedule);
    return schedule;
  }

  private Facility generateFacility() {
    Integer instanceNumber = this.getNextInstanceNumber();
    GeographicZone geographicZone = generateGeographicZone();
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("Facility" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + this.getNextInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone() {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + this.getNextInstanceNumber());
    geographicZone.setLevel(generateGeographicLevel());
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + this.getNextInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }
}