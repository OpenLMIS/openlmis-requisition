package org.openlmis.referencedata;

import com.google.gson.Gson;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Program;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class ProgramControllerIntegrationTest {

  private static final String RESOURCE_URL = "http://localhost:8080/api/programs";

  private Program program = new Program();

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    program.setCode("ProgramControllerIntegrationTest");
    program.setActive(true);
  }

  @Test
  public void testCreate() {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    Gson gson = new Gson();
    String json = gson.toJson(program);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Program> result = restTemplate.postForEntity(
        RESOURCE_URL, entity, Program.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Program savedProgram = result.getBody();

    Assert.assertNotNull(savedProgram.getId());
  }
}
