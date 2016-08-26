package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;
import org.openlmis.referencedata.repository.ProcessingScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.time.LocalDate;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class ProcessingScheduleControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/processingSchedules";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String DIFFERENCE_URL = RESOURCE_URL + "/{id}/difference";
  private static final String ACCESS_TOKEN = "access_token";

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

  @Autowired
  private ProcessingPeriodRepository periodRepository;

  private ProcessingSchedule schedule;
  private ProcessingPeriod period;

  @Before
  public void setUp() {
    schedule = new ProcessingSchedule();
    schedule.setCode("code");
    schedule.setName("schedule");
    schedule.setDescription("Test schedule");
    scheduleRepository.save(schedule);

    period = new ProcessingPeriod();
    period.setName("period");
    period.setProcessingSchedule(schedule);
    period.setDescription("Test period");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);
  }

  @Test
  public void shouldDisplayTotalDifference() {
    String response = restAssured.given()
        .pathParam("id", schedule.getId())
        .queryParam("access_token", getToken())
        .when()
        .get(DIFFERENCE_URL)
        .then()
        .statusCode(200)
        .extract().asString();

    assertTrue(response.contains("Period lasts 1 months and 0 days"));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteSchedule() {

    periodRepository.delete(period);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", schedule.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    Assert.assertFalse(scheduleRepository.exists(schedule.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateSchedule() {

    periodRepository.delete(period);
    scheduleRepository.delete(schedule);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(schedule)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateSchedule() {

    schedule.setDescription("OpenLMIS");

    ProcessingSchedule response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", schedule.getId())
          .body(schedule)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProcessingSchedule.class);

    assertEquals(response.getDescription(), "OpenLMIS");
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllSchedules() {

    ProcessingSchedule[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(ProcessingSchedule[].class);

    Iterable<ProcessingSchedule> schedules = Arrays.asList(response);
    assertTrue(schedules.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenSchedule() {

    ProcessingSchedule response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", schedule.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProcessingSchedule.class);

    assertTrue(scheduleRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
