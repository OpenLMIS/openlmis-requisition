package org.openlmis.referencedata.repository;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Before;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

@WebIntegrationTest("server.port:8080")
public class RequisitionTemplateRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RequisitionTemplate> {
  @Autowired
  RequisitionTemplateRepository repository;

  private static final String RESOURCE_URL_PROGRAMS = "http://localhost:8080/api/programs";
  private String requisitionTemplateRepository = "RequisitionTemplateRepositoryIntegrationTest";
  private Program program;
  private static int programsCounter = 0;

  @Before
  public void setUp() throws JsonProcessingException {
    program = new Program();
    program.setCode(requisitionTemplateRepository + "" + (programsCounter++));

    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(program);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Program> result = restTemplate.postForEntity(
            RESOURCE_URL_PROGRAMS, entity, Program.class);

    program = result.getBody();
  }

  RequisitionTemplateRepository getRepository() {
    return this.repository;
  }

  @Override
  RequisitionTemplate generateInstance() {
<<<<<<< HEAD
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(null);
=======
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(
            new HashMap<String, RequisitionTemplateColumn>());
>>>>>>> bfdbf05... OLMIS-386: Moved logic from controller to repository
    requisitionTemplate.setProgram(program);
    return requisitionTemplate;

  }


}
