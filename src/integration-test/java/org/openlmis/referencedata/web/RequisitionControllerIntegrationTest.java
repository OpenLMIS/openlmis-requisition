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
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Comment;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.CommentRepository;
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
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String requisitionRepositoryName = "RequisitionRepositoryIntegrationTest";
  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
      + "definition.";
  private static final String EXPECTED_MESSAGE_FIRST_PART = "{\n  \"requisitionLines\" : ";
  private final String insertComment = addTokenToUrl(BASE_URL + "/api/requisitions/{id}/comments");
  private final String approveRequisition =
      addTokenToUrl(BASE_URL + "/api/requisitions/{id}/approve");
  private final String skipUrl = addTokenToUrl(BASE_URL + "/api/requisitions/{id}/skip");
  private final String rejectUrl = addTokenToUrl(BASE_URL + "/api/requisitions/{id}/reject");
  private final String submitUrl = addTokenToUrl(BASE_URL + "/api/requisitions/{id}/submit");
  private final String submittedUrl = addTokenToUrl(BASE_URL + "/api/requisitions/submitted");
  private final String authorizationUrl = addTokenToUrl(
      BASE_URL + "/api/requisitions/{id}/authorize");
  private final String deleteUrl = addTokenToUrl(BASE_URL + "/api/requisitions/{id}");
  private final String searchUrl = addTokenToUrl(BASE_URL + "/api/requisitions/search");
  private final String initiateUrl = addTokenToUrl(BASE_URL + "/api/requisitions/initiate");

  private static final String COMMENT_TEXT_FIELD_NAME = "commentText";
  private static final String USERNAME = "testUser";

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
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

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
  private User user;

  /** Prepare the test environment. */
  @Before
  public void setUp() throws JsonProcessingException {
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();

    cleanUp();

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

    user = new User();
    user.setUsername(USERNAME);
    user.setLastName("LastnameTest");
    user.setFirstName("FirstnameTest");
    user.setActive(true);
    user.setVerified(true);
    userRepository.save(user);

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
    commentRepository.deleteAll();
    Iterable<User> users = userRepository.findByUsername(USERNAME);
    if (users != null && users.iterator().hasNext()) {
      userRepository.delete(users);
    }
    requisitionLineRepository.deleteAll();
    productRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    productCategoryRepository.deleteAll();
    configurationSettingRepository.deleteAll();
  }

  @Test
  public void testShouldSubmitCorrectRequisition() throws JsonProcessingException {
    testSubmit();
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullRequisitionLines()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
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

  @Test
  public void testShouldNotSubmitRequisitionWithNullQuantityInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A quantity must be entered prior to submission of a requisition.\"\n}";
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
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullBeginningBalanceInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A beginning balance must be entered prior to submission of a requisition.\"\n}";
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNegativeBeginningBalanceInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A beginning balance must be a non-negative value.\"\n}";
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(-1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullTotalReceivedQuantityInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A total received quantity must be entered prior to submission of a requisition.\"\n}";
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNegativeTotalReceivedQuantityInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A total received quantity must be a non-negative value.\"\n}";
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(-1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullStockHandInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A total stock on hand must be entered prior to submission of a requisition.\"\n}";
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullConsumedQuantityInRequisitionLinetest()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A total consumed quantity must be entered prior to submission of a requisition.\"\n}";
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLine.setStockOnHand(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);
    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullAttributesInRequisitionLine()
      throws JsonProcessingException {
    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
        + "\"A total losses and adjustments must be entered prior "
        + "to submission of a requisition.\"\n}";
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

    try {
      testSubmit();
      fail();
    } catch (HttpClientErrorException excp) {
      String response = excp.getResponseBodyAsString();
      assertEquals(expectedExceptionMessage, response);
    }
  }

  @Test
  public void testSkip() throws JsonProcessingException {
    restAssured.given()
            .contentType("application/json")
            .pathParam("id", requisition.getId())
            .when()
            .put(skipUrl)
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
            .put(rejectUrl)
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
    restTemplate.delete(deleteUrl, id);

    boolean exists = requisitionRepository.exists(id);
    Assert.assertFalse(exists);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testDeleteWithBadStatus() {
    UUID id = requisition.getId();
    RestTemplate restTemplate = new RestTemplate();

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    restTemplate.delete(deleteUrl, id);
  }

  private void testSubmit() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    mapper.registerModule(new Hibernate4Module());
    String json = mapper.writeValueAsString(requisition);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(submitUrl)
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
  public void testFindByNoParameter() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        searchUrl, HttpMethod.GET, null, new ParameterizedTypeReference<List<Requisition>>() {});
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(4, requisitions.size());
  }

  private void createComment(User author, Requisition req, String commentText) {
    Comment comment = new Comment();
    comment.setAuthor(author);
    comment.setRequisition(req);
    comment.setCommentText(commentText);
    commentRepository.save(comment);
  }

  @Test
  public void getCommentsForRequisitionTest() {
    createComment(user, requisition, "First comment");
    createComment(user, requisition, "Second comment");

    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(insertComment)
        .build().expand(requisition.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);
    ResponseEntity<Object> result =
        restTemplate.exchange(uri, HttpMethod.GET, entity, Object.class);

    List<LinkedHashMap<Object,Object>> comments =
        (List<LinkedHashMap<Object,Object>>) result.getBody();

    Assert.assertEquals("First comment", comments.get(0).get(COMMENT_TEXT_FIELD_NAME));
    Assert.assertEquals("Second comment", comments.get(1).get(COMMENT_TEXT_FIELD_NAME));
  }

  @Test
  public void insertCommentTest() throws JsonProcessingException {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    createComment(user, requisition, "Previous comment");
    Comment userPostComment = new Comment();
    userPostComment.setCommentText("User comment");

    ObjectMapper mapper = new ObjectMapper();
    mapper.registerModule(new Hibernate4Module());
    String json = mapper.writeValueAsString(userPostComment);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(insertComment)
        .build()
        .expand(requisition.getId().toString())
        .encode();
    String uri = uriComponents.toUriString();

    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<Object> result =
        restTemplate.exchange(uri, HttpMethod.POST, entity, Object.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
    List<LinkedHashMap<Object,Object>> comments =
        (List<LinkedHashMap<Object,Object>>) result.getBody();

    Assert.assertEquals("Previous comment", comments.get(0).get(COMMENT_TEXT_FIELD_NAME));
    Assert.assertEquals("User comment", comments.get(1).get(COMMENT_TEXT_FIELD_NAME));
  }

  private void approveRequisitionTest(Requisition requisition) {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(approveRequisition)
        .build().expand(requisition.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);
    ResponseEntity<Requisition> result =
        restTemplate.exchange(uri, HttpMethod.PUT, entity, Requisition.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
    Requisition approvedRequisition = result.getBody();
    Assert.assertNotNull(approvedRequisition.getId());
    Assert.assertEquals(requisition.getId(), approvedRequisition.getId());
    Assert.assertEquals(RequisitionStatus.APPROVED, approvedRequisition.getStatus());
  }

  @Test
  public void testApproveRequisition() {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);
    approveRequisitionTest(requisition);
  }

  @Test
  public void testApproveRequisitionSkippedAuthorization() {
    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    approveRequisitionTest(requisition);
  }

  @Test
  public void testFindByProgram() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<List<Requisition>> result = restTemplate.exchange(
        searchUrl + "&program={program}", HttpMethod.GET, null,
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
        searchUrl + "&facility={facility}", HttpMethod.GET, null,
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
        searchUrl + "&program={program}&facility={facility}", HttpMethod.GET, null,
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
        searchUrl + "&createdDateFrom=2015-03-04T12:00:00&createdDateTo=2016-01-04T12:00:00",
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
        searchUrl + "&program={program}&createdDateFrom=2015-06-20T12:00:00", HttpMethod.GET, null,
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
        searchUrl + "&facility={facility}&createdDateTo=2016-02-20T12:00:00", HttpMethod.GET, null,
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
        searchUrl + "&program={program}&facility={facility}&createdDateFrom=2015-03-20T12:00:00"
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
        searchUrl + "&facility={facility}&createdDateFrom=2015-06-20T12:00:00"
            + "&createdDateTo=2016-05-01T12:00:00", HttpMethod.GET, null,
        new ParameterizedTypeReference<List<Requisition>>() {}, facility2.getId());
    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    List<Requisition> requisitions = result.getBody();
    Assert.assertEquals(0, requisitions.size());
  }

  @Test
  public void testInitializeRequisition() throws JsonProcessingException {
    requisitionRepository.delete(requisition);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    mapper.registerModule(new Hibernate4Module());

    String json = mapper.writeValueAsString(requisition);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    RestTemplate restTemplate = new RestTemplate();

    ResponseEntity<Requisition> result = restTemplate.exchange(initiateUrl,
        HttpMethod.POST, entity, Requisition.class);

    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());
    Requisition initiatedRequisitions = result.getBody();
    Assert.assertNotNull(initiatedRequisitions);
  }

  @Test
  public void testGetSubmittedRequisitions() throws JsonProcessingException {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    RestTemplate restTemplate = new RestTemplate();

    ResponseEntity<Requisition[]> result = restTemplate.getForEntity(
        submittedUrl, Requisition[].class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
    Iterable<Requisition> requisitions = Arrays.asList(result.getBody());
    Assert.assertTrue(requisitions.iterator().hasNext());
  }

  @Test
  public void testAuthorize() throws JsonProcessingException {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    mapper.registerModule(new Hibernate4Module());

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    String json = mapper.writeValueAsString(requisition);

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(authorizationUrl)
        .build().expand(requisition.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<Requisition> result =
        restTemplate.exchange(uri, HttpMethod.PUT, entity, Requisition.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test(expected = HttpClientErrorException.class)
  public void testSkippedAuthorize() throws JsonProcessingException {
    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
    testAuthorize();
  }
}
