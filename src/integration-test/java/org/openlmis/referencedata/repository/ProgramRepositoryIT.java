package org.openlmis.referencedata.repository;

import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Program;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class ProgramRepositoryIT extends BaseCrudRepositoryIT<Program> {

  @Autowired
  ProgramRepository repository;

  ProgramRepository getRepository() {
    return this.repository;
  }

  Program generateInstance() {
    Program program = new Program();
    program.setCode(String.valueOf(this.getNextInstanceNumber()));
    return program;
  }
}
