package org.openlmis.referencedata.repository;



import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.openlmis.referencedata.domain.RequisitionTemplateColumn;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;
import java.util.Map;


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
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(new HashMap<String, RequisitionTemplateColumn>());
    requisitionTemplate.setProgram(program);
    return requisitionTemplate;
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrder() {
    String columnKey = "columnKey";
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn = new RequisitionTemplateColumn("name", "label", 1);
    columns.put(columnKey, testColumn);

    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate = repository.findOne(requisitionTemplate.getId());
    testColumn = requisitionTemplate.getColumnsMap().get(columnKey);
    assertEquals(1, testColumn.getDisplayOrder());

    requisitionTemplate.changeColumnDisplayOrder(columnKey, 2);
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate = repository.findOne(requisitionTemplate.getId());
    testColumn = requisitionTemplate.getColumnsMap().get(columnKey);
    assertEquals(2, testColumn.getDisplayOrder());
  }

}
