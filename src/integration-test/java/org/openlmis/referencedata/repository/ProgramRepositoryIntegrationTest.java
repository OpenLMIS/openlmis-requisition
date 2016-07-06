package org.openlmis.referencedata.repository;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.springframework.beans.factory.annotation.Autowired;

public class ProgramRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Program> {

  @Autowired
  ProgramRepository repository;

  ProgramRepository getRepository() {
    return this.repository;
  }

  Program generateInstance() {
    Program program = new Program();
    program.setCode(String.valueOf(this.getNextInstanceNumber()));
    program.setSkippable(true);
    return program;
  }

  @Test
  public void testSkippableEdit() {
    Program testProgram = this.generateInstance();
    testProgram = repository.save(testProgram);
    testProgram = repository.findOne(testProgram.getId());
    assertTrue(testProgram.getSkippable());

    testProgram.setSkippable(false);
    testProgram = repository.save(testProgram);
    testProgram = repository.findOne(testProgram.getId());
    assertFalse(testProgram.getSkippable());
  }
}
