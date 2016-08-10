package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.dto.ProgramDto;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.UUID;

import static org.junit.Assert.assertThat;

public class ProgramControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private ProgramRepository programRepository;

  private Program program = new Program();

  @Before
  public void setUp() {
    programRepository.deleteAll();
    program.setCode("code");
    program.setName("name");
    programRepository.save(program);
  }

  @After
  public void cleanup() {
    programRepository.deleteAll();
  }

  @Test
  public void testUpdate() {
    ProgramDto programDto = new ProgramDto(program.getId(), "newCode", "newName");

    Program response = restAssured.given()
        .queryParam("access_token", getToken())
        .contentType("application/json")
        .body(programDto)
        .when()
        .put("/api/programs/update")
        .then()
        .statusCode(200)
        .extract().as(Program.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(response.getCode(), "newCode");
    Assert.assertEquals(response.getName(), "newName");
  }

  @Test
  public void testUpdateIfProgramWithGivenIdNotExist() {
    ProgramDto programDto = new ProgramDto(UUID.randomUUID(), "new code", "new name");
    restAssured.given()
        .queryParam("access_token", getToken())
        .contentType("application/json")
        .body(programDto)
        .when()
        .put("/api/programs/update")
        .then()
        .statusCode(400);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testUpdateIfProgramIdIsNull() {
    ProgramDto programDto = new ProgramDto(null, "new code", "new name");
    restAssured.given()
        .queryParam("access_token", getToken())
        .contentType("application/json")
        .body(programDto)
        .when()
        .put("/api/programs/update")
        .then()
        .statusCode(400);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.responseChecks());
  }
}
