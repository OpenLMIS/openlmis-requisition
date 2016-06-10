package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.springframework.beans.factory.annotation.Autowired;

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
