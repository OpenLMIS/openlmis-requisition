package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

public class ScheduleControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private PeriodRepository periodRepository;

  private Schedule schedule;
  private Period period;

  @Before
  public void setUp() {
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

  @After
  public void cleanup() {
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
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
