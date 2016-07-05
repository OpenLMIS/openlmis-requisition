package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Requisition;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.RequisitionRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class RequisitionControllerTest {

  private static final String requisitionRepository = "RequisitionRepositoryIntegrationTest";
  private static final String RESOURCE_URL = "http://localhost:8080/api/requisitions/submit";

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  PeriodRepository periodRepository;

  @Autowired
  ScheduleRepository scheduleRepository;

  @Autowired
  FacilityRepository facilityRepository;

  @Autowired
  RequisitionRepository repository;

  private Program program = new Program();
  private Period period = new Period();
  private Schedule schedule = new Schedule();
  private Facility facility = new Facility();
  private Requisition requisition = new Requisition();

  @Before
  public void setUp() throws JsonProcessingException {

    schedule.setCode(requisitionRepository);
    schedule.setName(requisitionRepository);
    scheduleRepository.save(schedule);

    period.setName(requisitionRepository);
    period.setProcessingSchedule(schedule);
    period.setDescription(requisitionRepository);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);

    program.setCode(requisitionRepository);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionRepository);
    facilityType.setActive(true);

    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionRepository);
    level.setLevelNumber(1);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionRepository);
    geographicZone.setLevel(level);

    facility.setCode(requisitionRepository);
    facility.setGeographicZone(geographicZone);
    facility.setType(facilityType);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    repository.save(requisition);
  }

  @Test
  public void testSubmit() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    ObjectMapper mapper = new ObjectMapper();

    String json = mapper.writeValueAsString(requisition);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);;
    ResponseEntity<Requisition> result = restTemplate.postForEntity(RESOURCE_URL, entity, Requisition.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Requisition savedRequisition = result.getBody();
    Assert.assertNotNull(savedRequisition.getId());
  }
}
