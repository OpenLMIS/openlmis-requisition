package org.openlmis.requisition.web;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.jayway.restassured.RestAssured;
import com.jayway.restassured.config.ObjectMapperConfig;
import com.jayway.restassured.config.RestAssuredConfig;

import org.junit.After;
import org.junit.Rule;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.utils.CleanRepositoryHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;

import java.util.UUID;

@RunWith(SpringJUnit4ClassRunner.class)
@ActiveProfiles("test")
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public abstract class BaseWebIntegrationTest {
  protected static final UUID INITIAL_USER_ID = CleanRepositoryHelper.INITIAL_USER_ID;
  protected static final String RAML_ASSERT_MESSAGE =
      "HTTP request/response should match RAML definition.";

  protected static final String REFERENCEDATA_API_USERS = "/api/users/";
  protected static final String REFERENCEDATA_API_RIGHTS = "/api/rights/";

  protected RestAssuredClient restAssured;

  protected static final RamlDefinition ramlDefinition =
      RamlLoaders.fromClasspath().load("api-definition-raml.yaml").ignoringXheaders();

  protected static final String BASE_URL = System.getenv("BASE_URL");

  protected static final String UUID_REGEX =
      "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}";

  protected static final String CONTENT_TYPE = "Content-Type";

  protected static final String APPLICATION_JSON = "application/json";

  protected static final String FACILITY_ID = "1d5bdd9c-8702-11e6-ae22-56b6b6499611";

  protected static final String ACCESS_TOKEN = "access_token";

  private static final String MOCK_CHECK_RESULT = "{"
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

  private static final String MOCK_TOKEN_REQUEST_RESPONSE = "{"
      + "  \"access_token\": \"418c89c5-7f21-4cd1-a63a-38c47892b0fe\",\n"
      + "  \"token_type\": \"bearer\",\n"
      + "  \"expires_in\": 847,\n"
      + "  \"scope\": \"read write\",\n"
      + "  \"referenceDataUserId\": \"35316636-6264-6331-2d34-3933322d3462\"\n"
      + "}";

  private static final String MOCK_FIND_PROGRAM_RESULT = "{"
      + " \"id\":\"5c5a6f68-8658-11e6-ae22-56b6b6499611\","
      + " \"code\":\"Program Code\","
      + " \"name\":\"Program Name\","
      + " \"periodsSkippable\":true"
      + "}";

  private static final String MOCK_FIND_SUPPORTED_PROGRAM_RESULT = "{"
      + " \"id\":\"86191d25-4846-4775-a968-12df732e6004\","
      + " \"code\":\"Program Code\","
      + " \"name\":\"Program Name\","
      + " \"programActive\": true,"
      + " \"periodsSkippable\": false,"
      + " \"showNonFullSupplyTab\": false,"
      + " \"supportActive\": true,"
      + " \"supportStartDate\": \"2011-12-03\""
      + "}";

  private static final String MOCK_FIND_FACILITY_RESULT = "{"
      + " \"id\":\"" + FACILITY_ID + "\",\n"
      + " \"code\":\"facilityCode\",\n"
      + " \"name\":\"facility NameA\",\n"
      + " \"active\":true,\n"
      + " \"enabled\":true\n"
      + "}";

  private static final String MOCK_FIND_FACILITY_RESULT_WITH_SUPPORTED_PROGRAMS = "{"
      + " \"id\":\"aaf12a5a-8b16-11e6-ae22-56b6b6499611\",\n"
      + " \"name\":\"facility NameA\",\n"
      + " \"code\":\"facilityCode\",\n"
      + " \"active\":true,\n"
      + " \"enabled\":true,\n"
      + " \"supportedPrograms\": [" + MOCK_FIND_SUPPORTED_PROGRAM_RESULT + "]"
      + "}";

  private static final String MOCK_FIND_STOCK_ADJUSTMENT_REASONS_RESULT = "[{"
      + " \"id\":\"62c44f68-9200-1de2-22ea-34b5f98f121a\",\n"
      + " \"programId\":\"5c5a6f68-8658-11e6-ae22-56b6b6499611\",\n"
      + " \"additive\":\"true\",\n"
      + " \"displayOrder\":1\n"
      + "}]";

  private static final String MOCK_USER_SEARCH_RESULT = "[{"
      + "\"id\":\"35316636-6264-6331-2d34-3933322d3462\","
      + "\"username\":\"admin\","
      + "\"firstName\":\"Admin\","
      + "\"lastName\":\"User\","
      + "\"email\":\"example@mail.com\","
      + "\"verified\":false,"
      + "\"fulfillmentFacilities\": [" + MOCK_FIND_FACILITY_RESULT + "]"
      + "}]";

  private static final String MOCK_FIND_USER_RESULT = "{"
      + "\"id\":\"35316636-6264-6331-2d34-3933322d3462\","
      + "\"username\":\"admin\","
      + "\"firstName\":\"Admin\","
      + "\"lastName\":\"User\","
      + "\"email\":\"example@mail.com\","
      + "\"verified\":false,"
      + "\"fulfillmentFacilities\": [" + MOCK_FIND_FACILITY_RESULT + "]"
      + "}";

  private static final String MOCK_FIND_USER_SUPERVISED_PROGRAMS = "[{"
      + " \"id\":\"5c5a6f68-8658-11e6-ae22-56b6b6499611\""
      + "}]";

  private static final String MOCK_FIND_PRODUCT_RESULT = "{"
      + " \"id\":\"cd9e1412-8703-11e6-ae22-56b6b6499611\",\n"
      + " \"orderableCode\":\"Orderable Code\",\n"
      + " \"orderableName\":\"Orderable Name\",\n"
      + " \"packSize\":10,\n"
      + " \"packRoundingThreshold\":5,\n"
      + " \"roundToZero\":false\n"
      + "}";

  private static final String MOCK_FIND_PROCESSING_SCHEDULE = "{"
      + " \"id\":\"c73ad6a4-895c-11e6-ae22-56b6b6499611\","
      + " \"code\":\"Schedule Code\","
      + " \"name\":\"Schedule Name\""
      + "}";

  private static final String MOCK_SEARCH_PROCESSING_SCHEDULE = "["
      + MOCK_FIND_PROCESSING_SCHEDULE
      + "]";

  private static final String MOCK_FIND_PROCESSING_PERIOD = "{"
      + " \"id\":\"4c6b05c2-894b-11e6-ae22-56b6b6499611\","
      + " \"name\":\"Period Name\","
      + " \"description\":\"Period Description\","
      + "\"processingSchedule\":" + MOCK_FIND_PROCESSING_SCHEDULE + ","
      + " \"startDate\":\"2016-03-01\","
      + " \"endDate\":\"2017-03-01\","
      + " \"durationInMonths\":1"
      + " }";

  private static final String MOCK_FIND_PROGRAM_ORDERABLE = "{"
      + " \"id\":\"047cb32a-8962-11e6-ae22-56b6b6499611\","
      + " \"programId\": \"5c5a6f68-8658-11e6-ae22-56b6b6499611\","
      + " \"orderableId\": \"cd9e1412-8703-11e6-ae22-56b6b6499611\","
      + " \"ordereableDisplayCategoryId\": \"6d469a06-8962-11e6-ae22-56b6b6499611\","
      + " \"pricePerPack\": 13.77"
      + "}";

  private static final String MOCK_SEARCH_SUPPLY_LINE_RESULT = "[{\n"
      + " \"id\":\"99cd664e-871a-11e6-ae22-56b6b6499611\",\n"
      + " \"supervisoryNode\":\"aa66b244-871a-11e6-ae22-56b6b6499611\",\n"
      + " \"program\":\"aa66b58c-871a-11e6-ae22-56b6b6499611\",\n"
      + " \"supplyingFacility\":\"aa66b762-871a-11e6-ae22-56b6b6499611\"\n"
      + "}]";

  private static final String MOCK_SEARCH_APPROVED_PRODUCTS = "[{"
      + " \"id\":\"d0d5e0d6-8962-11e6-ae22-56b6b6499611\","
      + " \"programOrderable\":" + MOCK_FIND_PROGRAM_ORDERABLE + ","
      + " \"maxPeriodsOfStock\": 2"
      + "}]";

  private static final String MOCK_SEARCH_PROCESSING_PERIODS = "["
      + "" + MOCK_FIND_PROCESSING_PERIOD
      + "]";

  private static final String MOCK_SEARCH_FACILITIES_WITH_SIMILAR_CODE_OR_NAME = "["
      + "{"
      + " \"id\":\"aaf12a5a-8b16-11e6-ae22-56b6b6499611\",\n"
      + " \"code\":\"facilityCode\",\n"
      + " \"name\":\"facility NameA\",\n"
      + " \"active\":true,\n"
      + " \"enabled\":true\n"
      + "}"
      + "]";

  protected static final String MOCK_RIGHT_SEARCH = "["
      + "{"
      + "\"id\":\"00fb0d27-7ea7-4196-adf0-61103058e0e8\",\n"
      + "\"name\":\"rightName\"\n"
      + "}"
      + "]";

  private static final String MOCK_HAS_RIGHT = "{ \"result\":\"true\" }";

  protected static final String MOCK_SEARCH_SUPPLYING_FACILITY_RESULT = "["
      + MOCK_FIND_FACILITY_RESULT + "]";

  @Autowired
  private CleanRepositoryHelper cleanRepositoryHelper;

  @Autowired
  private ObjectMapper objectMapper;

  @Rule
  public WireMockRule wireMockRule = new WireMockRule(80);

  /**
   * Constructor for test.
   */
  public BaseWebIntegrationTest() {
    RestAssured.baseURI = BASE_URL;
    RestAssured.config = RestAssuredConfig.config().objectMapperConfig(
        new ObjectMapperConfig().jackson2ObjectMapperFactory((clazz, charset) -> objectMapper)
    );
    restAssured = ramlDefinition.createRestAssured();

    // This mocks the auth check to always return valid admin credentials.
    wireMockRule.stubFor(post(urlEqualTo("/api/oauth/check_token"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_CHECK_RESULT)));

    // This mocks the auth token request response
    wireMockRule.stubFor(post(urlPathEqualTo("/api/oauth/token?grant_type=client_credentials"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_TOKEN_REQUEST_RESPONSE)));

    // This mocks the call to auth to post to an auth user.
    wireMockRule.stubFor(post(urlPathEqualTo("/api/users"))
        .willReturn(aResponse()
            .withStatus(200)));

    // This mocks the call to notification to post a notification.
    wireMockRule.stubFor(post(urlPathEqualTo("/api/notification"))
        .willReturn(aResponse()
            .withStatus(200)));

    // This mocks searching for users
    wireMockRule.stubFor(post(urlMatching("/api/users/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_USER_SEARCH_RESULT)));

    // This mocks for find one user
    wireMockRule.stubFor(get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_USER_RESULT)));

    // This mocks the call to retrieve programs supervised by the user
    wireMockRule.stubFor(get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + "/programs.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_USER_SUPERVISED_PROGRAMS)));

    // This mocks the call to retrieve facilities supervised by the user
    wireMockRule.stubFor(get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX
        + "/supervisedFacilities.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody("[" + MOCK_FIND_FACILITY_RESULT_WITH_SUPPORTED_PROGRAMS + "]")));

    // This mocks the call to retrieve fulfillment facilities of the user
    wireMockRule.stubFor(
        get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + "/fulfillmentFacilities.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody("[" + MOCK_FIND_FACILITY_RESULT + "]")));

    // This mocks for find one program
    wireMockRule.stubFor(get(urlMatching("/api/programs/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PROGRAM_RESULT)));

    // This mocks for find stock adjustment reasons for program
    wireMockRule.stubFor(get(urlMatching("/api/stockAdjustmentReasons/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_STOCK_ADJUSTMENT_REASONS_RESULT)));

    // This mocks for find one facility
    wireMockRule.stubFor(get(urlMatching("/api/facilities/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_FACILITY_RESULT)));

    // This mocks for find one facility
    wireMockRule.stubFor(get(urlMatching("/api/facilities/aaf12a5a-8b16-11e6-ae22-56b6b6499611.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_FACILITY_RESULT_WITH_SUPPORTED_PROGRAMS)));

    // This mocks searching for approvedProducts
    wireMockRule.stubFor(get(urlMatching("/api/facilities/" + UUID_REGEX + "/approvedProducts.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_APPROVED_PRODUCTS)));

    // This mocks for find all orderable
    wireMockRule.stubFor(get(urlPathEqualTo("/api/orderables/"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody("[" + MOCK_FIND_PRODUCT_RESULT + "]")));

    // This mocks for find one orderable
    wireMockRule.stubFor(get(urlMatching("/api/orderables/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PRODUCT_RESULT)));

    // This mocks searching for supplying facilities
    wireMockRule.stubFor(get(urlMatching("/api/facilities/supplying.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_SUPPLYING_FACILITY_RESULT)));

    // This mocks searching for processingSchedules
    wireMockRule.stubFor(get(urlMatching("/api/processingSchedules/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_PROCESSING_SCHEDULE)));

    // This mocks retrieving single processing schedule
    wireMockRule.stubFor(get(urlMatching("/api/processingSchedules/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PROCESSING_SCHEDULE)));

    // This mocks searching for processingPeriods
    wireMockRule.stubFor(get(urlMatching("/api/processingPeriods/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_FIND_PROCESSING_PERIOD)));

    // This mocks searching for supplyLines
    wireMockRule.stubFor(get(urlMatching("/api/supplyLines/searchByUUID.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_SUPPLY_LINE_RESULT)));

    // This mocks searching for processingPeriods
    wireMockRule.stubFor(get(urlMatching("/api/processingPeriods/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_PROCESSING_PERIODS)));

    // This mocks searching for processingPeriods by UUID and date
    wireMockRule.stubFor(get(urlMatching("/api/processingPeriods/searchByUUIDAndDate.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_PROCESSING_PERIODS)));

    // This mocks searching facilities with similar facilityCode or facilityName
    wireMockRule.stubFor(get(urlMatching("/api/facilities/search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_SEARCH_FACILITIES_WITH_SIMILAR_CODE_OR_NAME)));

    // This mocks for checking if a user has a right
    wireMockRule.stubFor(get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + "/hasRight.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_HAS_RIGHT))
    );

    // This mocks searching for right by name
    wireMockRule.stubFor(get(urlMatching(REFERENCEDATA_API_RIGHTS + "search.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(MOCK_RIGHT_SEARCH))
    );
  }

  @After
  public void cleanRepositories() {
    cleanRepositoryHelper.cleanAll();
  }

  protected String getToken() {
    return "418c89c5-7f21-4cd1-a63a-38c47892b0fe";
  }

  public UUID getSharedFacilityId() {
    return UUID.fromString("aaf12a5a-8b16-11e6-ae22-56b6b6499611");
  }
}
