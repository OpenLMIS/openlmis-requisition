package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.dto.ProgramDto;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class ProgramControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String UPDATE_URL = "/api/programs/update";

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
  public void testUpdate() {
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
  public void testUpdateIfProgramWithGivenIdNotExist() {
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
  public void testUpdateIfProgramIdIsNull() {
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
}
