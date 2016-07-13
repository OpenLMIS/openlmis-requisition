package org.openlmis.referencedata.web;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.dto.ProgramDto;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public class ProgramControllerIntegrationTest {

  private static final String UPDATE_URL = "http://localhost:8080/api/programs/update";

  @Autowired
  private ProgramRepository programRepository;

  private Program program = new Program();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    programRepository.deleteAll();
    program.setCode("code");
    program.setName("name");
    programRepository.save(program);
  }

  /**
   * Cleanup the test environment.
   */
  @After
  public void cleanup() {
    programRepository.deleteAll();
  }

  @Test
  public void testUpdate() {
    ProgramDto programDto = new ProgramDto(program.getId(), "newCode", "newName");
    ResponseEntity<Program> response = updateProgram(programDto);

    Program program = response.getBody();

    Assert.assertEquals(response.getStatusCode(), HttpStatus.OK);
    Assert.assertEquals(program.getCode(), "newCode");
    Assert.assertEquals(program.getName(), "newName");
  }

  @Test(expected = HttpClientErrorException.class)
  public void testUpdateIfProgramWithGivenIdNotExist() {
    ProgramDto programDto = new ProgramDto(UUID.randomUUID(), "new code", "new name");
    updateProgram(programDto);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testUpdateIfProgramIdIsNull() {
    ProgramDto programDto = new ProgramDto(null, "new code", "new name");
    updateProgram(programDto);
  }

  private ResponseEntity<Program> updateProgram(ProgramDto programDto) {
    RestTemplate restTemplate = new RestTemplate();
    return restTemplate.postForEntity(UPDATE_URL, programDto, Program.class);
  }
}
