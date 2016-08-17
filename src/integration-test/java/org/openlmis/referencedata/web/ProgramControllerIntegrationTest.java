package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.dto.ProgramDto;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class ProgramControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String RESOURCE_URL = "/api/programs";
  private static final String UPDATE_URL = RESOURCE_URL + "/update";
  private static final String DELETE_OR_GET_URL = RESOURCE_URL + "/{id}";

  @Autowired
  private ProgramRepository programRepository;

  private Program program = new Program();

  @Before
  public void setUp() {
    program.setCode("code");
    program.setName("name");
    programRepository.save(program);
  }

  @Test
  public void testShouldUpdate() {
    ProgramDto programDto = new ProgramDto(program.getId(), "newCode", "newName");

    Program response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(programDto)
        .when()
        .put(UPDATE_URL)
        .then()
        .statusCode(200)
        .extract().as(Program.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(response.getCode(), "newCode");
    assertEquals(response.getName(), "newName");
  }

  @Test
  public void testShouldNotUpdateIfProgramWithGivenIdNotExist() {
    ProgramDto programDto = new ProgramDto(UUID.randomUUID(), "new code", "new name");
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(programDto)
        .when()
        .put(UPDATE_URL)
        .then()
        .statusCode(400);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotUpdateIfProgramIdIsNull() {
    ProgramDto programDto = new ProgramDto(null, "new code", "new name");
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(programDto)
        .when()
        .put(UPDATE_URL)
        .then()
        .statusCode(400);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.responseChecks());
  }

  @Test
  public void testShouldDeleteProgram() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", program.getId())
          .when()
          .delete(DELETE_OR_GET_URL)
          .then()
          .statusCode(204);

    assertFalse(programRepository.exists(program.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldCreateProgram() {

    programRepository.delete(program);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(program)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldGetAllProgram() {

    Program[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Program[].class);

    Iterable<Program> programs = Arrays.asList(response);
    assertTrue(programs.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldGetChoosenProgram() {

    Program response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", program.getId())
          .when()
          .get(DELETE_OR_GET_URL)
          .then()
          .statusCode(200)
          .extract().as(Program.class);

    assertTrue(programRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
