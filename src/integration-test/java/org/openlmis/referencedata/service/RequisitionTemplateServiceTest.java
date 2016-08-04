package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class RequisitionTemplateServiceTest {

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

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
  public void cleanup() {
    requisitionTemplateRepository.deleteAll();
    programRepository.deleteAll();
  }

  @Test
  public void testSearchRequisitionTemplates() {
    List<RequisitionTemplate> receivedRequisitionTemplates
        = requisitionTemplateService.searchRequisitionTemplates(requisitionTemplate.getProgram());
    Assert.assertEquals(1,receivedRequisitionTemplates.size());

    Assert.assertEquals(
        requisitionTemplate.getProgram().getId(),
        receivedRequisitionTemplates.get(0).getProgram().getId());
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
