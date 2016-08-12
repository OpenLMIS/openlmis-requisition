package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.core.JsonProcessingException;
import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.time.LocalDate;

public class PeriodControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/periods";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String DIFFERENCE_URL = RESOURCE_URL + "/{id}/difference";
  private static final String PROCESSING_SCHEDULE = "processingSchedule";
  private static final String START_DATE = "toDate";
  private static final String ACCESS_TOKEN = "access_token";

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private PeriodRepository periodRepository;

  private Period firstPeriod = new Period();
  private Period secondPeriod = new Period();
  private Schedule schedule = new Schedule();

  @Before
  public void setUp() {
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

  @Test
  public void testCreatePeriodsWithoutGap() throws JsonProcessingException {
    firstPeriod.setProcessingSchedule(schedule);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(firstPeriod)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    secondPeriod.setProcessingSchedule(schedule);

    Period savedPeriod = restAssured.given()
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(secondPeriod)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201)
        .extract().as(Period.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertNotNull(savedPeriod.getId());
  }

  @Test
  public void testCreatePeriodsWithAGap() throws JsonProcessingException {
    schedule.setCode("newCode");
    schedule.setName("newSchedule");
    scheduleRepository.save(schedule);

    firstPeriod.setProcessingSchedule(schedule);
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(firstPeriod)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    secondPeriod.setStartDate(LocalDate.of(2016, 2, 3));
    secondPeriod.setEndDate(LocalDate.of(2016, 3, 2));
    secondPeriod.setProcessingSchedule(schedule);

    restAssured.given()
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(secondPeriod)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testGetTotalDifference() {
    firstPeriod.setProcessingSchedule(schedule);
    periodRepository.save(firstPeriod);

    String response = restAssured.given()
        .pathParam("id", firstPeriod.getId())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(DIFFERENCE_URL)
        .then()
        .statusCode(200)
        .extract().asString();

    assertTrue(response.contains("Period lasts 1 months and 1 days"));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testSearchPeriods() {
    firstPeriod.setProcessingSchedule(schedule);
    firstPeriod.setStartDate(LocalDate.now().plusDays(1));
    periodRepository.save(firstPeriod);
    secondPeriod.setProcessingSchedule(schedule);
    secondPeriod.setStartDate(LocalDate.now());
    periodRepository.save(secondPeriod);

    Period[] response = restAssured.given()
        .queryParam(PROCESSING_SCHEDULE, firstPeriod.getProcessingSchedule().getId())
        .queryParam(START_DATE, firstPeriod.getStartDate().toString())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(Period[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(1, response.length);
    for ( Period period : response ) {
      Assert.assertEquals(
          period.getProcessingSchedule().getId(),
          firstPeriod.getProcessingSchedule().getId());
      Assert.assertTrue(period.getStartDate().isBefore(firstPeriod.getStartDate()));
    }
  }
}
