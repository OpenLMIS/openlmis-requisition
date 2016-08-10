package org.openlmis.referencedata.repository;



import static org.junit.Assert.assertEquals;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/** Allow testing requisitionTemplateRepository. */

public class RequisitionTemplateRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RequisitionTemplate> {

  private static final String REQUISITION_TEMPLATE_REPOSITORY =
          "RequisitionTemplateRepositoryIntegrationTest";
  private static final String COLUMN_KEY = "columnKey";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  @Autowired
  RequisitionTemplateRepository repository;

  @Autowired
  ProgramRepository programRepository;

  private Program program = new Program();
  private List<RequisitionTemplate> requisitionTemplates;

  @Before
  public void setUp() {
    requisitionTemplates = new ArrayList<>();
    program.setCode(REQUISITION_TEMPLATE_REPOSITORY);
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
                    "name", "label", 1, true, true, true, true, SOURCE);
    columns.put(COLUMN_KEY, testColumn);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(1, testColumn.getDisplayOrder());
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_KEY, 2);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(2, testColumn.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrderWithCanChangeOrderFalse() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn1", "testLabelColumn1", 1,
                false, false, false, false, SourceType.CALCULATED);
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_KEY,2);
    assertEquals(1, testColumn1.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateDisplayStatus() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn2", "testLabelColumn2", 1,
                false, false, false, false, SourceType.CALCULATED);
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY, true);
    assertEquals(false, testColumn1.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
    testColumn1.setIsDisplayRequired(true);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY,false);
    assertEquals(true, testColumn1.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
  }

  @Test
  public void testChangeRequisitionTemplateLabel() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
            new RequisitionTemplateColumn(
                    "testNameColumn3", "testLabelColumn3", 1,
                false, false, false, false, SourceType.CALCULATED);
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    requisitionTemplate.changeColumnLabel(COLUMN_KEY, "newLabel");
    assertEquals(testColumn1.getLabel(),"newLabel");

  }

  @Test
  public void testChangeRequisitionTemplateSource() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column =
            new RequisitionTemplateColumn("name", "label", 1,
                false, false, false, false, SourceType.CALCULATED);
    columns.put(COLUMN_KEY, column);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(column.getSource(), SOURCE);
    requisitionTemplate.changeColumnSource(COLUMN_KEY, SourceType.USER_INPUT);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(column.getSource(), SourceType.USER_INPUT);
  }

  @Test
  public void testIsProductCodeFirstWhenDisplayed() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column =
            new RequisitionTemplateColumn("productCode", "label", 2,
                false, false, false, false, SourceType.CALCULATED);
    columns.put(COLUMN_KEY, column);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(column.getDisplayOrder(), 2);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY, true);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(column.getDisplayOrder(), 1);
  }

  @Test
  public void testSearchRequisitionTemplates() {
    for (int reqTemplateCount = 0; reqTemplateCount < 5; reqTemplateCount++) {
      RequisitionTemplate requisitionTemplate = generateInstance();
      requisitionTemplate.setProgram(generateProgram());
      requisitionTemplates.add(repository.save(requisitionTemplate));
    }
    List<RequisitionTemplate> receivedRequisitionTemplates
            = repository.searchRequisitionTemplates(requisitionTemplates.get(0).getProgram());

    Assert.assertEquals(1, receivedRequisitionTemplates.size());
    for (RequisitionTemplate requisitionTemplate : receivedRequisitionTemplates) {
      Assert.assertEquals(
              requisitionTemplates.get(0).getProgram().getId(),
              requisitionTemplate.getProgram().getId());
    }
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("code" + this.getNextInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }
}
