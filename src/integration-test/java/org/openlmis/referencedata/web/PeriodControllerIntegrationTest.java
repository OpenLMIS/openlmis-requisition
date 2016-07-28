package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDate;

public class PeriodControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private PeriodRepository periodRepository;

  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
          + "definition.";
  private final String resourceUrl = addTokenToUrl(BASE_URL + "/api/periods");

  private Period firstPeriod = new Period();
  private Period secondPeriod = new Period();
  private Schedule schedule = new Schedule();

  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  @Before
  public void setUp() {
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();

    cleanup();

    schedule.setCode("code");
    schedule.setName("schedule");
    schedule.setDescription("Test schedule");
    scheduleRepository.save(schedule);
    firstPeriod.setName("period");
    firstPeriod.setDescription("Test period");
    firstPeriod.setStartDate(LocalDate.of(2016, 1, 1));
    firstPeriod.setEndDate(LocalDate.of(2016, 2, 1));
    secondPeriod.setName("period");
    secondPeriod.setDescription("Test period");
    secondPeriod.setStartDate(LocalDate.of(2016, 2, 2));
    secondPeriod.setEndDate(LocalDate.of(2016, 3, 2));
  }

  @After
  public void cleanup() {
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
  }

  @Test
  public void testCreatePeriodsWithoutGap() throws JsonProcessingException {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    RestTemplate restTemplate = new RestTemplate();

    ObjectMapper mapper = new ObjectMapper();

    firstPeriod.setProcessingSchedule(schedule);

    String firstPeriodJson = mapper.writeValueAsString(firstPeriod);

    HttpEntity<String> periodEntity = new HttpEntity<>(firstPeriodJson, headers);

    restTemplate.postForEntity(resourceUrl, periodEntity, Period.class);

    secondPeriod.setProcessingSchedule(schedule);

    String secondPeriodJson = mapper.writeValueAsString(secondPeriod);

    HttpEntity<String> secondPeriodEntity = new HttpEntity<>(secondPeriodJson, headers);

    ResponseEntity<Period> result = restTemplate.postForEntity(
            resourceUrl, secondPeriodEntity, Period.class);

    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());
    Period savedPeriod = result.getBody();
    Assert.assertNotNull(savedPeriod.getId());
  }

  @Test(expected = HttpClientErrorException.class)
  public void testCreatePeriodsWithAGap() throws JsonProcessingException {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    schedule.setCode("newCode");
    schedule.setName("newSchedule");
    scheduleRepository.save(schedule);

    firstPeriod.setProcessingSchedule(schedule);

    ObjectMapper mapper = new ObjectMapper();
    String firstPeriodJson = mapper.writeValueAsString(firstPeriod);

    HttpEntity<String> periodEntity = new HttpEntity<>(firstPeriodJson, headers);

    RestTemplate restTemplate = new RestTemplate();
    restTemplate.postForEntity(resourceUrl, periodEntity, Period.class);

    secondPeriod.setStartDate(LocalDate.of(2016, 2, 3));
    secondPeriod.setEndDate(LocalDate.of(2016, 3, 2));
    secondPeriod.setProcessingSchedule(schedule);

    String secondPeriodJson = mapper.writeValueAsString(secondPeriod);

    HttpEntity<String> secondPeriodEntity = new HttpEntity<>(secondPeriodJson, headers);

    restTemplate.postForEntity(resourceUrl, secondPeriodEntity, Period.class);
  }

  @Test
  public void testGetTotalDifference() {
    firstPeriod.setProcessingSchedule(schedule);
    periodRepository.save(firstPeriod);

    String response = restAssured.given()
            .pathParam("id", firstPeriod.getId())
            .queryParam("access_token", getToken())
            .when()
            .get("/api/periods/{id}/difference")
            .then()
            .statusCode(200)
            .extract().asString();

    assertTrue(response.contains("Period lasts 1 months and 1 days"));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
