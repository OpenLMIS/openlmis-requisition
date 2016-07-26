package org.openlmis.referencedata.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.hibernate4.Hibernate4Module;
import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
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
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest {

  private static final String requisitionRepositoryName = "RequisitionRepositoryIntegrationTest";
  private static final String BASE_URL = System.getenv("BASE_URL");
  private static final String SUBMIT_URL = BASE_URL + "/api/requisitions/{id}/submit";
  private static final String SKIP_URL = BASE_URL + "/api/requisitions/{id}/skip";
  private static final String AUTHORIZATION_URL = BASE_URL + "/api/requisitions/{id}/authorize";
  private static final String REJECT_URL = BASE_URL + "/api/requisitions/{id}/reject";
  private static final String DELETE_URL = BASE_URL + "/api/requisitions/{id}";
  private static final String CREATED_BY_LOGGED_USER_URL = BASE_URL 
      + "/api/requisitions/creator/{creatorId}";
  private static final String SEARCH_URL = BASE_URL + "/api/requisitions/search";
  private static final String INITIATE_URL = BASE_URL + "/api/requisitions/initiate";
  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
          + "definition.";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  ProductCategoryRepository productCategoryRepository;

  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  private Requisition requisition = new Requisition();
  private Requisition requisition2 = new Requisition();
  private Requisition requisition3 = new Requisition();
  private Requisition requisition4 = new Requisition();
  private Period period = new Period();
  private Product product = new Product();
  private Program program = new Program();
  private Program program2 = new Program();
  private Facility facility = new Facility();
  private Facility facility2 = new Facility();
  private User user = new User();

  /** Prepare the test environment. */
  @Before
  public void setUp() throws JsonProcessingException {
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();

    cleanUp();

    user.setUsername("testUser");
    user.setPassword("password");
    user.setFirstName("Test");
    user.setLastName("User");
    userRepository.save(user);

    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

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
    product.setProductCategory(productCategory1);
    productRepository.save(product);

    program.setCode(requisitionRepositoryName);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    program2.setCode(requisitionRepositoryName + "2");
    program2.setPeriodsSkippable(true);
    programRepository.save(program2);

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

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionRepositoryName);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    FacilityType facilityType2 = new FacilityType();
    facilityType2.setCode(requisitionRepositoryName + "2");
    facilityTypeRepository.save(facilityType2);

    GeographicLevel level2 = new GeographicLevel();
    level2.setCode(requisitionRepositoryName + "2");
    level2.setLevelNumber(1);
    geographicLevelRepository.save(level2);

    GeographicZone geographicZone2 = new GeographicZone();
    geographicZone2.setCode(requisitionRepositoryName + "2");
    geographicZone2.setLevel(level2);
    geographicZoneRepository.save(geographicZone2);

    facility2.setType(facilityType2);
    facility2.setGeographicZone(geographicZone2);
    facility2.setCode(requisitionRepositoryName + "2");
    facility2.setActive(true);
    facility2.setEnabled(true);
    facilityRepository.save(facility2);

    Schedule schedule = new Schedule();
    schedule.setCode(requisitionRepositoryName);
    schedule.setName(requisitionRepositoryName);
    scheduleRepository.save(schedule);

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
    requisition.setStatus(RequisitionStatus.INITIATED);

    requisitionRepository.save(requisition);

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    requisition2.setFacility(facility2);
    requisition2.setProcessingPeriod(period);
    requisition2.setProgram(program);
    requisition2.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition2);
    requisition2.setCreatedDate(LocalDateTime.parse("2015-04-01T12:00:00"));
    requisitionRepository.save(requisition2);

    requisition3.setFacility(facility);
    requisition3.setProcessingPeriod(period);
    requisition3.setProgram(program2);
    requisition3.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition3);
    requisition3.setCreatedDate(LocalDateTime.parse("2015-12-01T12:00:00"));
    requisitionRepository.save(requisition3);

    requisition4.setFacility(facility2);
    requisition4.setProcessingPeriod(period);
    requisition4.setProgram(program2);
    requisition4.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition4);
    requisition4.setCreatedDate(LocalDateTime.parse("2015-02-01T12:00:00"));
    requisitionRepository.save(requisition4);
  }

  /**
   * Cleanup the test environment.
   */
  @After
  public void cleanUp() {
    requisitionLineRepository.deleteAll();
    productRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    userRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testCorrectSubmit() throws JsonProcessingException {
    testSubmit();
  }

  @Test
  public void testSubmitWithNullRequisitionLines() throws JsonProcessingException {
    String expectedExceptionMessage = "{\n  \"requisitionLines\" : "
        + "\"A requisitionLines must be entered prior to submission of a requisition.\"\n}";
    requisition.setRequisitionLines(null);
    requisition = requisitionRepository.save(requisition);

    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test(expected = HttpClientErrorException.class)
  public void testSubmitWithIncorrectRequisitionLines() throws JsonProcessingException {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    testSubmit();
  }

  @Test(expected = HttpClientErrorException.class)
  public void testSubmitWithRequisitionLinesNullAttributes() throws JsonProcessingException {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(null);
    requisitionLine.setTotalConsumedQuantity(null);
    requisitionLine.setBeginningBalance(null);
    requisitionLine.setTotalReceivedQuantity(null);
    requisitionLine.setTotalLossesAndAdjustments(null);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    testSubmit();
  }

  @Test
  public void testSkip() throws JsonProcessingException {
    restAssured.given()
            .contentType("application/json")
            .pathParam("id", requisition.getId())
            .when()
            .put(SKIP_URL)
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testReject() throws JsonProcessingException {

    requisition.setRequisitionLines(null);
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    restAssured.given()
            .contentType("application/json")
            .pathParam("id", requisition.getId())
            .when()
            .put(REJECT_URL)
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
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

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(SUBMIT_URL)
        .build()
        .expand(requisition.getId().toString())
        .encode();
    String uri = uriComponents.toUriString();

    ResponseEntity<Requisition> result =
        restTemplate.exchange(uri, HttpMethod.PUT, entity, Requisition.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
    Requisition savedRequisition = result.getBody();
    Assert.assertNotNull(savedRequisition.getId());
    Assert.assertEquals(requisition.getId(), savedRequisition.getId());
    Assert.assertEquals(RequisitionStatus.SUBMITTED, savedRequisition.getStatus());
  }

  @Test
  public void testSearchByCreatorId() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result =
        restTemplate.exchange(CREATED_BY_LOGGED_USER_URL, HttpMethod.GET, null, 
            new ParameterizedTypeReference<List<Requisition>>() {
            }, user.getId());

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(1, requisitions.size());
  }

  @Test
  public void testFindByNoParameter() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL, HttpMethod.GET, null, new ParameterizedTypeReference<List<Requisition>>() {});
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(4, requisitions.size());
  }

  @Test
  public void testFindByProgram() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?program={program}", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, program.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(2, requisitions.size());

    for (Requisition r : requisitions) {
      Assert.assertEquals(program.getId(), r.getProgram().getId());
    }
  }

  @Test
  public void testFindByFacility() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?facility={facility}", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, facility2.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(2, requisitions.size());

    for (Requisition r : requisitions) {
      Assert.assertEquals(facility2.getId(), r.getFacility().getId());
    }
  }

  @Test
  public void testFindByProgramAndFacility() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?program={program}&facility={facility}", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {},
        program2.getId(), facility2.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(1, requisitions.size());
    Requisition req = requisitions.get(0);
    Assert.assertEquals(program2.getId(), req.getProgram().getId());
    Assert.assertEquals(facility2.getId(), req.getFacility().getId());
  }

  @Test
  public void testFindByCreatedDateRange() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?createdDateFrom=2015-03-04T12:00:00&createdDateTo=2016-01-04T12:00:00",
        HttpMethod.GET, null, new ParameterizedTypeReference<List<Requisition>>() {});
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(2, requisitions.size());

    for (Requisition r : requisitions) {
      assertTrue(r.getCreatedDate().isAfter(LocalDateTime.parse("2015-03-04T12:00:00")));
      assertTrue(r.getCreatedDate().isBefore(LocalDateTime.parse("2016-01-04T12:00:00")));
    }
  }

  @Test
  public void testFindByProgramAndCreatedDate() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?program={program}&createdDateFrom=2015-06-20T12:00:00", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, program.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(1, requisitions.size());
    Requisition req = requisitions.get(0);
    Assert.assertEquals(program.getId(), req.getProgram().getId());
    assertTrue(req.getCreatedDate().isAfter(LocalDateTime.parse("2015-06-20T12:00:00")));
  }

  @Test
  public void testFindByFacilityAndCreatedDate() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?facility={facility}&createdDateTo=2016-02-20T12:00:00", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, facility.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(1, requisitions.size());
    Requisition req = requisitions.get(0);
    Assert.assertEquals(facility.getId(), req.getFacility().getId());
    assertTrue(req.getCreatedDate().isBefore(LocalDateTime.parse("2016-02-20T12:00:00")));
  }

  @Test
  public void testFindByAllParameters() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?program={program}&facility={facility}&createdDateFrom=2015-03-20T12:00:00"
            + "&createdDateTo=2015-05-01T12:00:00", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, program.getId(), facility2.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(1, requisitions.size());
    Requisition req = requisitions.get(0);
    Assert.assertEquals(program.getId(), req.getProgram().getId());
    Assert.assertEquals(facility2.getId(), req.getFacility().getId());
    assertTrue(req.getCreatedDate().isAfter(LocalDateTime.parse("2015-03-20T12:00:00")));
    assertTrue(req.getCreatedDate().isBefore(LocalDateTime.parse("2015-05-01T12:00:00")));
  }

  @Test
  public void testFindEmptyResult() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        SEARCH_URL + "?facility={facility}&createdDateFrom=2015-06-20T12:00:00"
            + "&createdDateTo=2016-05-01T12:00:00", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, facility2.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(0, requisitions.size());
  }

  @Test
  public void testInitializeRequisition() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<Requisition> result = restTemplate.exchange(
        INITIATE_URL + "?facilityId={facilityId}&"
                + "programId={programId}&periodId={periodId}&emergency=true",
        HttpMethod.POST, null, Requisition.class, facility2.getId(),
            program.getId(), period.getId());

    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());
    Requisition initiatedRequisitions = result.getBody();
    Assert.assertNotNull(initiatedRequisitions);
  }

  @Test
  public void testAuthorize() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(AUTHORIZATION_URL)
        .build().expand(requisition.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);

    ResponseEntity<Object> result =
        restTemplate.exchange(uri, HttpMethod.PUT, entity, Object.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
  }
}
