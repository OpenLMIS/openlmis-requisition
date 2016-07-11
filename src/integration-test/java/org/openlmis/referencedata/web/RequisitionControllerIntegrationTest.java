package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.product.domain.Product;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public class RequisitionControllerIntegrationTest {

  private static final String requisitionRepositoryName = "RequisitionRepositoryIntegrationTest";
  private static final String RESOURCE_URL = "http://localhost:8080/api/requisitions/submit";
  private static final String SKIP_URL = "http://localhost:8080/api/requisitions/skip";


  @Autowired
  ProductRepository productRepository;

  @Autowired
  RequisitionLineRepository requisitionLineRepository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  PeriodRepository periodRepository;

  @Autowired
  ScheduleRepository scheduleRepository;

  @Autowired
  FacilityRepository facilityRepository;

  @Autowired
  RequisitionRepository requisitionRepository;

  private Requisition requisition = new Requisition();
  private Product product = new Product();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() throws JsonProcessingException {
    requisitionLineRepository.deleteAll();
    productRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();

    product.setCode(requisitionRepositoryName);
    product.setPrimaryName(requisitionRepositoryName);
    product.setDispensingUnit(requisitionRepositoryName);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    productRepository.save(product);

    Program program = new Program();
    program.setCode(requisitionRepositoryName);
    program.setSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionRepositoryName);
    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionRepositoryName);
    level.setLevelNumber(1);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionRepositoryName);
    geographicZone.setLevel(level);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionRepositoryName);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setCode(requisitionRepositoryName);
    schedule.setName(requisitionRepositoryName);
    scheduleRepository.save(schedule);

    Period period = new Period();
    period.setName(requisitionRepositoryName);
    period.setProcessingSchedule(schedule);
    period.setDescription(requisitionRepositoryName);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);

    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);

    requisitionRepository.save(requisition);

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequisition(requisition);
    requisitionLine.setProduct(product);
    requisitionLine.setQuantityRequested(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
  }

  @Test
  public void testCorrectSubmit() throws JsonProcessingException {
    testSubmit();
  }

  @Test(expected = HttpClientErrorException.class)
  public void testSubmitWithNullRequisitionLines() throws JsonProcessingException {
    requisition.setRequisitionLines(null);
    requisition = requisitionRepository.save(requisition);
    testSubmit();
  }

  @Test(expected = HttpClientErrorException.class)
  public void testSubmitWithIncorrectRequisitionLines() throws JsonProcessingException {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequisition(requisition);
    requisitionLine.setProduct(product);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    testSubmit();
  }

  @Test
  public void testSkip() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(requisition);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Requisition> result = restTemplate.postForEntity(
        SKIP_URL, entity, Requisition.class);
    Assert.assertEquals(HttpStatus.ACCEPTED, result.getStatusCode());
  }

  private void testSubmit() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(requisition);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Requisition> result = restTemplate.postForEntity(
        RESOURCE_URL, entity, Requisition.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Requisition savedRequisition = result.getBody();
    Assert.assertNotNull(savedRequisition.getId());
    Assert.assertEquals(requisition.getId(), savedRequisition.getId());
    Assert.assertEquals(RequisitionStatus.SUBMITTED, savedRequisition.getStatus());
  }

}
