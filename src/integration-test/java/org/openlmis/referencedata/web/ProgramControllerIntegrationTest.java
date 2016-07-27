package org.openlmis.referencedata.web;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.dto.ProgramDto;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;

public class ProgramControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String UPDATE_URL = BASE_URL + "/api/programs/update";

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
    return restTemplate.postForEntity(addTokenToUrl(UPDATE_URL), programDto, Program.class);
  }
}
