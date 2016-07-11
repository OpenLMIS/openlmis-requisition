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
  private String columnKey = "columnKey";
  private String source = "source";

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
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn =
            new RequisitionTemplateColumn(
                    "name", "label", 1,true,true,true, source);
    columns.put(columnKey, testColumn);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn = requisitionTemplate.getColumnsMap().get(columnKey);
    assertEquals(1, testColumn.getDisplayOrder());
    requisitionTemplate.changeColumnDisplayOrder(columnKey, 2);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn = requisitionTemplate.getColumnsMap().get(columnKey);
    assertEquals(2, testColumn.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrderWithCanChangeOrderFalse() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn1", "testLabelColumn1", 1, false, false, false, source);
    columns.put(columnKey, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(columnKey);
    requisitionTemplate.changeColumnDisplayOrder(columnKey,2);
    assertEquals(1, testColumn1.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateDisplayStatus() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn2", "testLabelColumn2", 1, false, false, false, source);
    columns.put(columnKey, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate = repository.findOne(requisitionTemplate.getId());
    testColumn1 = requisitionTemplate.getColumnsMap().get(columnKey);
    requisitionTemplate.changeColumnDisplay(columnKey, true);

    assertEquals(false, testColumn1.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
    testColumn1.setIsDisplayRequired(true);
    requisitionTemplate.changeColumnDisplay(columnKey,false);
    assertEquals(true, testColumn1.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
  }

  @Test
  public void testChangeRequisitionTemplateLabel() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn3", "testLabelColumn3", 1, false, false, false, source);
    columns.put(columnKey, testColumn1);


    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(columnKey);

    requisitionTemplate.changeColumnLabel(columnKey, "newLabel");
    assertEquals(testColumn1.getLabel(),"newLabel");

  }

  @Test
  public void testChangeRequisitionTemplateSource() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column =
            new RequisitionTemplateColumn("name", "label", 1, false, false, false, source);
    columns.put(columnKey, column);

    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(columnKey);

    requisitionTemplate.changeColumnSource(columnKey, "newSource");
    assertEquals(column.getSource(), "newSource");
  }

  @Test
  public void testChangeRequisitionTemplateIsDisplayedChangeDisplayOrder() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column =
            new RequisitionTemplateColumn("productCode", "label", 2, false, false, false, source);
    columns.put(columnKey, column);

    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(columnKey);
    assertEquals(column.getDisplayOrder(), 2);

    requisitionTemplate.changeColumnDisplay(columnKey, true);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(columnKey);
    assertEquals(column.getDisplayOrder(), 1);
  }
}
