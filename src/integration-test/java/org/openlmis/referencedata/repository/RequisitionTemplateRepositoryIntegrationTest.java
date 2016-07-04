package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
/** Allow testing requisitionTemplateRepository. */

public class RequisitionTemplateRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RequisitionTemplate> {
  @Autowired
  RequisitionTemplateRepository repository;

  private String requisitionTemplateRepository = "RequisitionTemplateRepositoryIntegrationTest";

  @Autowired
  ProgramRepository programRepository;

  private Program program;

  private static int programsCounter = 0;

  /** Allow setup environment before test. */
  @Before
  public void setUp() {
    program = new Program();
    program.setCode(requisitionTemplateRepository + "" + (programsCounter++));
    programRepository.save(program);
  }

  RequisitionTemplateRepository getRepository() {
    return this.repository;
  }

  RequisitionTemplate generateInstance() {
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    return requisitionTemplate;
  }
}