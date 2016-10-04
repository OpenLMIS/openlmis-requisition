package org.openlmis.requisition.web;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.jayway.restassured.RestAssured;

import org.junit.After;
import org.junit.Rule;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.utils.CleanRepositoryHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.UUID;

import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public abstract class BaseWebIntegrationTest {
  protected static final UUID INITIAL_USER_ID = CleanRepositoryHelper.INITIAL_USER_ID;
  protected static final String RAML_ASSERT_MESSAGE =
      "HTTP request/response should match RAML definition.";

  protected RestAssuredClient restAssured;

  protected static final RamlDefinition ramlDefinition =
      RamlLoaders.fromClasspath().load("api-definition-raml.yaml");

  protected static final String BASE_URL = System.getenv("BASE_URL");

  protected static final String UUID_REGEX =
      "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}";

  protected static final String CONTENT_TYPE = "Content-Type";

  protected static final String APPLICATION_JSON = "application/json";

  private static final String MOCK_CHECK_RESULT = "{\n"
      + "  \"aud\": [\n"
      + "    \"auth\",\n"
      + "    \"example\",\n"
      + "    \"requisition\",\n"
      + "    \"notification\",\n"
      + "    \"referencedata\"\n"
      + "  ],\n"
      + "  \"user_name\": \"admin\",\n"
      + "  \"referenceDataUserId\": \"35316636-6264-6331-2d34-3933322d3462\",\n"
      + "  \"scope\": [\n"
      + "    \"read\",\n"
      + "    \"write\"\n"
      + "  ],\n"
      + "  \"exp\": 1474500343,\n"
      + "  \"authorities\": [\n"
      + "    \"USER\",\n"
      + "    \"ADMIN\"\n"
      + "  ],\n"
      + "  \"client_id\": \"trusted-client\"\n"
      + "}";

  private static final String MOCK_TOKEN_REQUEST_RESPONSE = "{\n"
      + "  \"access_token\": \"418c89c5-7f21-4cd1-a63a-38c47892b0fe\",\n"
      + "  \"token_type\": \"bearer\",\n"
      + "  \"expires_in\": 847,\n"
      + "  \"scope\": \"read write\",\n"
      + "  \"referenceDataUserId\": \"35316636-6264-6331-2d34-3933322d3462\"\n"
      + "}";

  private static final String MOCK_USER_SEARCH_RESULT = "[{"
      + "\"id\":\"35316636-6264-6331-2d34-3933322d3462\","
      + "\"username\":\"admin\","
      + "\"firstName\":\"Admin\","
      + "\"lastName\":\"User\","
      + "\"email\":\"example@mail.com\","
      + "\"verified\":false"
      + "}]";

  private static final String MOCK_FIND_USER_RESULT = "{"
      + "\"id\":\"35316636-6264-6331-2d34-3933322d3462\","
      + "\"username\":\"admin\","
      + "\"firstName\":\"Admin\","
      + "\"lastName\":\"User\","
      + "\"email\":\"example@mail.com\","
      + "\"verified\":false"
      + "}";

  private static final String MOCK_FIND_PROGRAM_RESULT = "{"
      + " \"id\":\"5c5a6f68-8658-11e6-ae22-56b6b6499611\","
      + " \"code\":\"Program Code\","
      + " \"name\":\"Program Name\","
      + " \"periodsSkippable\":true"
      + "}";

  private static final String MOCK_FIND_FACILITY_RESULT = "{\n"
      + " \"id\":\"1d5bdd9c-8702-11e6-ae22-56b6b6499611\",\n"
      + " \"code\":\"Facility Code\",\n"
      + " \"active\":true,\n"
      + " \"enabled\":true\n"
      + "}";

  private static final String MOCK_FIND_PRODUCT_RESULT = "{"
      + " \"id\":\"cd9e1412-8703-11e6-ae22-56b6b6499611\",\n"
      + " \"productCode\":\"Product Code\",\n"
      + " \"productName\":\"Product Name\"\n"
      + "}";

  private static final String MOCK_FIND_PROCESSING_SCHEDULE = "{"
      + " \"id\":\"c73ad6a4-895c-11e6-ae22-56b6b6499611\","
      + " \"code\":\"Schedule Code\","
      + " \"name\":\"Schedule Name\""
      + "}";

  private static final String MOCK_FIND_PROCESSING_PERIOD = "{"
      + " \"id\":\"4c6b05c2-894b-11e6-ae22-56b6b6499611\","
      + " \"name\":\"Period Name\","
      + " \"description\":\"Period Description\","
      +  "\"processingSchedule\":" + MOCK_FIND_PROCESSING_SCHEDULE + ","
      + " \"startDate\":\"2016-03-01\","
      + " \"endDate\":\"2017-03-01\""
      + " }";

  private static final String MOCK_FIND_FACILITY_TYPE = "{"
      + " \"id\":\"7fbef45e-8961-11e6-ae22-56b6b6499611\","
      + " \"code\":\"Facility Type Code\""
      + "}";

  private static final String MOCK_FIND_PRODUCT_CATEGORY = "{"
      + " \"id\":\"6d469a06-8962-11e6-ae22-56b6b6499611\""
      + "}";

  private static final String MOCK_FIND_PROGRAM_PRODUCT = "{"
      + " \"id\":\"047cb32a-8962-11e6-ae22-56b6b6499611\","
      + " \"program\":" + MOCK_FIND_PROGRAM_RESULT + ","
      + " \"product\":" + MOCK_FIND_PRODUCT_RESULT + ","
      + " \"productCategory\":" + MOCK_FIND_PRODUCT_CATEGORY
      + "}";

  private static final String MOCK_SEARCH_SUPPLY_LINE_RESULT = "[{\n"
      + " \"id\":\"99cd664e-871a-11e6-ae22-56b6b6499611\",\n"
      + " \"supervisoryNode\":\"aa66b244-871a-11e6-ae22-56b6b6499611\",\n"
      + " \"program\":\"aa66b58c-871a-11e6-ae22-56b6b6499611\",\n"
      + " \"supplyingFacility\":\"aa66b762-871a-11e6-ae22-56b6b6499611\"\n"
      + "}]";

  private static final String MOCK_SEARCH_REQUISITION_GROUP_PROGRAM_SCHEDULE = "{"
      + " \"id\":\"7b34f06e-895c-11e6-ae22-56b6b6499611\","
      + " \"processingSchedule\":" + MOCK_FIND_PROCESSING_SCHEDULE
      + "}";

  private static final String MOCK_SEARCH_FACILITY_TYPE_APPROVED_PRODUCTS = "[{"
      + " \"id\":\"d0d5e0d6-8962-11e6-ae22-56b6b6499611\","
      + " \"facilityType\":" + MOCK_FIND_FACILITY_TYPE + ","
      + " \"programProduct\":" + MOCK_FIND_PROGRAM_PRODUCT + ","
      + " \"maxMonthStock\": 2"
      + "}]";

  private static final String MOCK_SEARCH_PROCESSING_PERIODS = "["
      + "" + MOCK_FIND_PROCESSING_PERIOD
      + "]";

  @Autowired
  private CleanRepositoryHelper cleanRepositoryHelper;

  @Rule
  public WireMockRule wireMockRule = new WireMockRule(80);

  /**
   * Constructor for test.
   */
  public BaseWebIntegrationTest() {
    RestAssured.baseURI = BASE_URL;
    restAssured = ramlDefinition.createRestAssured();

    // This mocks the auth check to always return valid admin credentials.
    wireMockRule.stubFor(post(urlEqualTo("/auth/oauth/check_token"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_CHECK_RESULT)));

    // This mocks the auth token request response
    wireMockRule.stubFor(post(urlPathEqualTo("/auth/oauth/token?grant_type=client_credentials"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_TOKEN_REQUEST_RESPONSE)));

    // This mocks the call to auth to post to an auth user.
    wireMockRule.stubFor(post(urlPathEqualTo("/api/users"))
        .willReturn(aResponse()
            .withStatus(200)));

    // This mocks the call to notification to post a notification.
    wireMockRule.stubFor(post(urlPathEqualTo("/notification"))
        .willReturn(aResponse()
            .withStatus(200)));

    // This mocks searching for users
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/users/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_USER_SEARCH_RESULT)));

    // This mocks for find one user
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/users/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_USER_RESULT)));

    // This mocks for find one program
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/programs/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PROGRAM_RESULT)));

    // This mocks for find one facility
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/facilities/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_FACILITY_RESULT)));

    // This mocks for find one orderableproduct
    wireMockRule.stubFor(get(
        urlMatching("/referencedata/api/orderableProducts/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PRODUCT_RESULT)));

    // This mocks searching for processingPeriods
    wireMockRule.stubFor(get(
        urlMatching("/referencedata/api/processingPeriods/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PROCESSING_PERIOD)));

    // This mocks searching for supplyLines
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/supplyLines/searchByUUID.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_SUPPLY_LINE_RESULT)));

    // This mocks searching for requisitionGroupProgramSchedules
    wireMockRule.stubFor(get(
        urlMatching("/referencedata/api/requisitionGroupProgramSchedules/searchByUUIDs.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_REQUISITION_GROUP_PROGRAM_SCHEDULE)));

    // This mocks searching for facilityTypeApprovedProducts
    wireMockRule.stubFor(get(
        urlMatching("/referencedata/api/facilityTypeApprovedProducts/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_FACILITY_TYPE_APPROVED_PRODUCTS)));

    // This mocks searching for processingPeriods
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/processingPeriods/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_PROCESSING_PERIODS)));

    // This mocks searching for processingPeriods by UUIDs
    wireMockRule.stubFor(get(urlMatching("/referencedata/api/processingPeriods/searchByUUIDs.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_PROCESSING_PERIODS)));

  }

  @After
  public void cleanRepositories() {
    cleanRepositoryHelper.cleanAll();
  }

  protected String getToken() {
    return "418c89c5-7f21-4cd1-a63a-38c47892b0fe";
  }

  public UUID getUserId() {
    return UUID.fromString("35316636-6264-6331-2d34-3933322d3462");
  }

  public UUID getProgramId() {
    return UUID.fromString("aa66b58c-871a-11e6-ae22-56b6b6499611");
  }

  public UUID getProcessingPeriodId() {
    return UUID.fromString("4c6b05c2-894b-11e6-ae22-56b6b6499611");
  }

  public UUID getProcessingScheduleId() {
    return UUID.fromString("c73ad6a4-895c-11e6-ae22-56b6b6499611");
  }
}
