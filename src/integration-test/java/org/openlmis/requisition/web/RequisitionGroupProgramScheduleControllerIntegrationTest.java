//package org.openlmis.requisition.web;
//
//import guru.nidi.ramltester.junit.RamlMatchers;
//import org.junit.Before;
//import org.junit.Test;
//import org.openlmis.hierarchyandsupervision.domain.RequisitionGroupProgramSchedule;
//import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
//import org.openlmis.referencedata.domain.Program;
//import org.openlmis.referencedata.domain.ProcessingSchedule;
//import org.openlmis.requisition.repository.ProgramRepository;
//import org.openlmis.requisition.repository.ProcessingScheduleRepository;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.http.MediaType;
//
//import java.util.Arrays;
//import java.util.UUID;
//
//import static org.junit.Assert.assertFalse;
//import static org.junit.Assert.assertThat;
//import static org.junit.Assert.assertTrue;
//
//public class RequisitionGroupProgramScheduleControllerIntegrationTest
//      extends BaseWebIntegrationTest {
//
//  private static final String RESOURCE_URL = "/api/requisitionGroupProgramSchedules";
//  private static final String ID_URL = RESOURCE_URL + "/{id}";
//  private static final String ACCESS_TOKEN = "access_token";
//  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
//
//  @Autowired
//  private RequisitionGroupProgramScheduleRepository repository;
//
//  @Autowired
//  private ProcessingScheduleRepository scheduleRepository;
//
//  @Autowired
//  private ProgramRepository programRepository;
//
//  private RequisitionGroupProgramSchedule reqGroupProgSchedule =
//        new RequisitionGroupProgramSchedule();
//
//  @Before
//  public void setUp() {
//    ProcessingSchedule schedule = new ProcessingSchedule();
//    schedule.setCode("scheduleCode");
//    schedule.setName("scheduleName");
//    scheduleRepository.save(schedule);
//
//    Program program = new Program();
//    program.setCode("programCode");
//    program.setPeriodsSkippable(true);
//    programRepository.save(program);
//
//    reqGroupProgSchedule.setDirectDelivery(false);
//    reqGroupProgSchedule.setProcessingSchedule(schedule);
//    reqGroupProgSchedule.setProgram(program);
//    repository.save(reqGroupProgSchedule);
//  }
//
//  @Test
//  public void shouldDeleteRequisitionGroupProgramSchedule() {
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", reqGroupProgSchedule.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(204);
//
//    assertFalse(repository.exists(reqGroupProgSchedule.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteNonexistentRequisitionGroupProgramSchedule() {
//
//    repository.delete(reqGroupProgSchedule);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", reqGroupProgSchedule.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateRequisitionGroupProgramSchedule() {
//
//    repository.delete(reqGroupProgSchedule);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .body(reqGroupProgSchedule)
//          .when()
//          .post(RESOURCE_URL)
//          .then()
//          .statusCode(201);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetAllRequisitionGroupsProgramSchedule() {
//
//    RequisitionGroupProgramSchedule[] response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .when()
//          .get(RESOURCE_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionGroupProgramSchedule[].class);
//
//    Iterable<RequisitionGroupProgramSchedule> requisitionGroupProgramSchedules =
//          Arrays.asList(response);
//    assertTrue(requisitionGroupProgramSchedules.iterator().hasNext());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetChosenRequisitionGroupProgramSchedule() {
//
//    RequisitionGroupProgramSchedule response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", reqGroupProgSchedule.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionGroupProgramSchedule.class);
//
//    assertTrue(repository.exists(response.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotGetNonexistentRequisitionGroupProgramSchedule() {
//
//    repository.delete(reqGroupProgSchedule);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", reqGroupProgSchedule.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionGroupProgramSchedule() {
//
//    reqGroupProgSchedule.setDirectDelivery(true);
//
//    RequisitionGroupProgramSchedule response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", reqGroupProgSchedule.getId())
//          .body(reqGroupProgSchedule)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionGroupProgramSchedule.class);
//
//    assertTrue(response.isDirectDelivery());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateNewRequisitionGroupProgramScheduleIfDoesNotExist() {
//
//    repository.delete(reqGroupProgSchedule);
//    reqGroupProgSchedule.setDirectDelivery(true);
//
//    RequisitionGroupProgramSchedule response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", ID)
//          .body(reqGroupProgSchedule)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionGroupProgramSchedule.class);
//
//    assertTrue(response.isDirectDelivery());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//}
