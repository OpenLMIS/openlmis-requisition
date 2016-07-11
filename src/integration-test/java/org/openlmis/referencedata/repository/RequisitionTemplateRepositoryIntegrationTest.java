package org.openlmis.referencedata.repository;



import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
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
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(
            new HashMap<String, RequisitionTemplateColumn>());
    requisitionTemplate.setProgram(program);
    return requisitionTemplate;
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrder() {
    String columnKey = "columnKey";
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn =
            new RequisitionTemplateColumn(
                    "name", "label", 1,true,true,true);
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

  @Test
  public void testChangeRequisitionTemplateColumnOrderWithCanChangeOrderFalse() {
    String column1Key = "columnKey";
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn1", "testLabelColumn1", 1, false, false, false);
    columns.put(column1Key, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate = repository.findOne(requisitionTemplate.getId());
    testColumn1 = requisitionTemplate.getColumnsMap().get(column1Key);
    requisitionTemplate.changeColumnDisplayOrder(column1Key,2);
    assertEquals(1, testColumn1.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateDisplayStatus() {
    String column1Key = "columnKey";
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn2", "testLabelColumn2", 1, false, false, false);
    columns.put(column1Key, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate = repository.findOne(requisitionTemplate.getId());
    testColumn1 = requisitionTemplate.getColumnsMap().get(column1Key);
    requisitionTemplate.changeColumnDisplay(column1Key, true);
    assertEquals(false, testColumn1.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
    testColumn1.setIsDisplayRequired(true);
    requisitionTemplate.changeColumnDisplay(column1Key,false);
    assertEquals(true, testColumn1.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
  }

  @Test
  public void testChangeRequisitionTemplateName() {
    String column1Key = "columnKeyKey";
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn3", "testLabelColumn3", 1, false, false, false);
    columns.put(column1Key, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate = repository.findOne(requisitionTemplate.getId());
    testColumn1 = requisitionTemplate.getColumnsMap().get(column1Key);
    requisitionTemplate.changeColumnName("columnKeyKey", "newName");
    assertEquals(testColumn1.getName(),"newName");

  }
}
