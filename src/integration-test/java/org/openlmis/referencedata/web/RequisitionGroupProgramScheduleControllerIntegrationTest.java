package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.RequisitionGroupProgramSchedule;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.Arrays;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class RequisitionGroupProgramScheduleControllerIntegrationTest
      extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitionGroupProgramSchedules";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";

  @Autowired
  private RequisitionGroupProgramScheduleRepository repository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private ProgramRepository programRepository;

  private RequisitionGroupProgramSchedule reqGroupProgSchedule =
        new RequisitionGroupProgramSchedule();

  @Before
  public void setUp() {
    Schedule schedule = new Schedule();
    schedule.setCode("scheduleCode");
    schedule.setName("scheduleName");
    scheduleRepository.save(schedule);

    Program program = new Program();
    program.setCode("programCode");
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    reqGroupProgSchedule.setDirectDelivery(false);
    reqGroupProgSchedule.setProcessingSchedule(schedule);
    reqGroupProgSchedule.setProgram(program);
    repository.save(reqGroupProgSchedule);
  }

  @Test
  public void testShouldDeleteRequisitionGroupProgramSchedule() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", reqGroupProgSchedule.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(repository.exists(reqGroupProgSchedule.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldCreateRequisitionGroupProgramSchedule() {

    repository.delete(reqGroupProgSchedule);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(reqGroupProgSchedule)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldGetAllRequisitionGroupsProgramSchedule() {

    RequisitionGroupProgramSchedule[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionGroupProgramSchedule[].class);

    Iterable<RequisitionGroupProgramSchedule> requisitionGroupProgramSchedules =
          Arrays.asList(response);
    assertTrue(requisitionGroupProgramSchedules.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldGetChosenRequisitionGroupProgramSchedule() {

    RequisitionGroupProgramSchedule response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", reqGroupProgSchedule.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionGroupProgramSchedule.class);

    assertTrue(repository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
