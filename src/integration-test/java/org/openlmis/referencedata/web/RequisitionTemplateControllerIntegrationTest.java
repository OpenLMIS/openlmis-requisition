package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class RequisitionTemplateControllerIntegrationTest {

  private static final String RESOURCE_URL = "http://localhost:8080/api/requisitionTemplates";
  private static final String RESOURCE_URL_PROGRAMS = "http://localhost:8080/api/programs";

  private RequisitionTemplate requisitionTemplate;

  private String requisitionTemplateController = "RequisitionTemplateControllerIntegrationTest";

  //** Prepare the rest environment. *//*
  @Before
  public void setUp() throws JsonProcessingException {
    Program program = new Program();
    program.setCode(requisitionTemplateController);

    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(program);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Program> result = restTemplate.postForEntity(
            RESOURCE_URL_PROGRAMS, entity, Program.class);

    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(result.getBody());
  }

  @Test
  public void testCreate() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(requisitionTemplate);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<RequisitionTemplate> result = restTemplate.postForEntity(
            RESOURCE_URL, entity, RequisitionTemplate.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    RequisitionTemplate saverRequisitionTemplate = result.getBody();

    Assert.assertNotNull(saverRequisitionTemplate.getId());
  }
}
