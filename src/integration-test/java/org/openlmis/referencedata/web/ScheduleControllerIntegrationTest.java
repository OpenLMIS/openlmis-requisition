package org.openlmis.referencedata.web;

import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class ScheduleControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML " 
      + "definition.";

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private PeriodRepository periodRepository;

  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  private Schedule schedule;
  private Period period;

  @Before
  public void setUp() {
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();

    schedule = new Schedule();
    schedule.setCode("code");
    schedule.setName("schedule");
    schedule.setDescription("Test schedule");
    scheduleRepository.save(schedule);

    period = new Period();
    period.setName("period");
    period.setProcessingSchedule(schedule);
    period.setDescription("Test period");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);
  }

  @Test
  public void testGetTotalDifference() {
    String response = restAssured.given()
        .pathParam("id", schedule.getId())
        .queryParam("access_token", getToken())
        .when()
        .get("/api/schedules/{id}/difference")
        .then()
        .statusCode(200)
        .extract().asString();

    assertTrue(response.contains("Period lasts 1 months and 0 days"));
    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
