//package org.openlmis.requisition.web;
//
//import guru.nidi.ramltester.junit.RamlMatchers;
//import org.junit.Before;
//import org.junit.Test;
//import org.openlmis.referencedata.domain.Program;
//import org.openlmis.requisition.repository.ProgramRepository;
//import org.openlmis.requisition.domain.RequisitionTemplate;
//import org.openlmis.requisition.repository.RequisitionTemplateRepository;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.http.MediaType;
//
//import java.util.Arrays;
//import java.util.UUID;
//
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertFalse;
//import static org.junit.Assert.assertThat;
//import static org.junit.Assert.assertTrue;
//
//@SuppressWarnings("PMD.TooManyMethods")
//public class RequisitionTemplateControllerIntegrationTest extends BaseWebIntegrationTest {
//
//  private static final String RESOURCE_URL = "/api/requisitionTemplates";
//  private static final String SEARCH_URL = RESOURCE_URL + "/search";
//  private static final String ID_URL = RESOURCE_URL + "/{id}";
//  private static final String ACCESS_TOKEN = "access_token";
//  private static final String PROGRAM = "program";
//  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
//
//  @Autowired
//  private ProgramRepository programRepository;
//
//  @Autowired
//  private RequisitionTemplateRepository requisitionTemplateRepository;
//
//  private RequisitionTemplate requisitionTemplate;
//  private Integer currentInstanceNumber;
//
//  @Before
//  public void setUp() {
//    currentInstanceNumber = 0;
//    requisitionTemplate = generateRequisitionTemplate();
//  }
//
//  @Test
//  public void shouldFindRequisitionTemplates() {
//    RequisitionTemplate[] response = restAssured.given()
//        .queryParam(PROGRAM, requisitionTemplate.getProgram().getId())
//        .queryParam(ACCESS_TOKEN, getToken())
//        .when()
//        .get(SEARCH_URL)
//        .then()
//        .statusCode(200)
//        .extract().as(RequisitionTemplate[].class);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//    assertEquals(1, response.length);
//    for ( RequisitionTemplate responseRequisitionTemplate : response ) {
//      assertEquals(
//          requisitionTemplate.getProgram().getId(),
//          responseRequisitionTemplate.getProgram().getId());
//      assertEquals(
//          requisitionTemplate.getId(),
//          responseRequisitionTemplate.getId());
//    }
//  }
//
//  @Test
//  public void shouldDeleteRequisitionTemplate() {
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionTemplate.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(204);
//
//    assertFalse(requisitionTemplateRepository.exists(requisitionTemplate.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteNonexistentRequisitionTemplate() {
//
//    requisitionTemplateRepository.delete(requisitionTemplate);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionTemplate.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//
//  @Test
//  public void shouldCreateRequisitionTemplate() {
//
//    requisitionTemplateRepository.delete(requisitionTemplate);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .body(requisitionTemplate)
//          .when()
//          .post(RESOURCE_URL)
//          .then()
//          .statusCode(201);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionTemplate() {
//
//    Program program = generateProgram();
//    requisitionTemplate.setProgram(program);
//
//    RequisitionTemplate response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionTemplate.getId())
//          .body(requisitionTemplate)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionTemplate.class);
//
//    assertEquals(response.getProgram().getId(), program.getId());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateNewRequisitionTemplateIfDoesNotExist() {
//
//    requisitionTemplateRepository.delete(requisitionTemplate);
//    Program program = generateProgram();
//    requisitionTemplate.setProgram(program);
//
//    RequisitionTemplate response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", ID)
//          .body(requisitionTemplate)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionTemplate.class);
//
//    assertEquals(response.getProgram().getId(), program.getId());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetAllRequisitionTemplates() {
//
//    RequisitionTemplate[] response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .when()
//          .get(RESOURCE_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionTemplate[].class);
//
//    Iterable<RequisitionTemplate> requisitionTemplates = Arrays.asList(response);
//    assertTrue(requisitionTemplates.iterator().hasNext());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetChosenRequisitionTemplate() {
//
//    RequisitionTemplate response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionTemplate.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionTemplate.class);
//
//    assertTrue(requisitionTemplateRepository.exists(response.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotGetNonexistentRequisitionTemplate() {
//
//    requisitionTemplateRepository.delete(requisitionTemplate);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionTemplate.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  private RequisitionTemplate generateRequisitionTemplate() {
//    RequisitionTemplate reqTemplate = new RequisitionTemplate();
//    reqTemplate.setProgram(generateProgram());
//    requisitionTemplateRepository.save(reqTemplate);
//    return reqTemplate;
//  }
//
//  private Program generateProgram() {
//    Program program = new Program();
//    program.setCode("code" + generateInstanceNumber());
//    program.setPeriodsSkippable(false);
//    programRepository.save(program);
//    return program;
//  }
//
//  private Integer generateInstanceNumber() {
//    currentInstanceNumber += 1;
//    return currentInstanceNumber;
//  }
//}
