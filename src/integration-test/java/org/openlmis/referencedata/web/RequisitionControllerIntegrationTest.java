package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.hibernate4.Hibernate4Module;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
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
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public class RequisitionControllerIntegrationTest {

  private static final String requisitionRepositoryName = "RequisitionRepositoryIntegrationTest";
  private static final String SUBMIT_URL = "http://localhost:8080/api/requisitions/submit";
  private static final String SKIP_URL = "http://localhost:8080/api/requisitions/{id}/skip";
  private static final String REJECT_URL = "http://localhost:8080/api/requisitions/{id}/reject";
  private static final String DELETE_URL = "http://localhost:8080/api/requisitions/{id}";
  private static final String CREATED_BY_LOGGED_USER_URL = "http://localhost:8080/api/requisitions/creator/{creatorId}";

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

  @Autowired
  UserRepository userRepository;

  private Requisition requisition = new Requisition();
  private Product product = new Product();
  private Program program = new Program();
  private Facility facility = new Facility();
  private User user = new User();

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
    userRepository.deleteAll();

    user.setUsername("testUser");
    user.setPassword("password");
    user.setFirstName("Test");
    user.setLastName("User");
    userRepository.save(user);

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

    requisition.setCreator(user);
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

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(SKIP_URL)
      .build()
      .expand(requisition.getId().toString())
      .encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);

    ResponseEntity<Object> result = restTemplate.exchange(uri, HttpMethod.PUT, entity, Object.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
  }
  @Test
  public void testReject() throws JsonProcessingException {

    requisition.setRequisitionLines(null);
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(REJECT_URL)
        .build()
        .expand(requisition.getId().toString())
        .encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);
    ResponseEntity<Object> response = restTemplate.exchange(uri, HttpMethod.PUT, entity, Object.class);

    Assert.assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void testDelete() {
    UUID id = requisition.getId();
    RestTemplate restTemplate = new RestTemplate();

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);
    restTemplate.delete(DELETE_URL, id);

    boolean exists = requisitionRepository.exists(id);
    Assert.assertFalse(exists);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testDeleteWithBadStatus() {
    UUID id = requisition.getId();
    RestTemplate restTemplate = new RestTemplate();

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    restTemplate.delete(DELETE_URL, id);
  }

  private void testSubmit() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    mapper.registerModule(new Hibernate4Module());

    String json = mapper.writeValueAsString(requisition);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Requisition> result = restTemplate.postForEntity(
        SUBMIT_URL, entity, Requisition.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Requisition savedRequisition = result.getBody();
    Assert.assertNotNull(savedRequisition.getId());
    Assert.assertEquals(requisition.getId(), savedRequisition.getId());
    Assert.assertEquals(RequisitionStatus.SUBMITTED, savedRequisition.getStatus());
  }

  @Test
  public void testSearchByCreatorId() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(CREATED_BY_LOGGED_USER_URL, HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, user.getId());

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(1, requisitions.size());
  }
}
