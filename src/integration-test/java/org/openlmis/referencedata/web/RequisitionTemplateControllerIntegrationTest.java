package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;

public class RequisitionTemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitionTemplates";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String PROGRAM = "program";

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  private RequisitionTemplate requisitionTemplate;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    requisitionTemplate = generateRequisitionTemplate();
  }

  @After
  public void cleanUp() {
    requisitionTemplateRepository.deleteAll();
    programRepository.deleteAll();
  }

  @Test
  public void testSearchRequisitionTemplates() {
    RequisitionTemplate[] response = restAssured.given()
        .queryParam(PROGRAM, requisitionTemplate.getProgram().getId())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(1, response.length);
    for ( RequisitionTemplate responseRequisitionTemplate : response ) {
      Assert.assertEquals(
          requisitionTemplate.getProgram().getId(),
          responseRequisitionTemplate.getProgram().getId());
      Assert.assertEquals(
          requisitionTemplate.getId(),
          responseRequisitionTemplate.getId());
    }
  }

  private RequisitionTemplate generateRequisitionTemplate() {
    RequisitionTemplate reqTemplate = new RequisitionTemplate();
    reqTemplate.setProgram(generateProgram());
    requisitionTemplateRepository.save(reqTemplate);
    return reqTemplate;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("code" + generateInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
