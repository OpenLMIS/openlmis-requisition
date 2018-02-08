/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.repository;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;


/**
 * Allow testing requisitionTemplateRepository.
 */

public class RequisitionTemplateRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionTemplate> {

  private static final String COLUMN_KEY = "columnKey";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  @Autowired
  RequisitionTemplateRepository repository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  @Autowired
  private AvailableRequisitionColumnOptionRepository availableRequisitionColumnOptionRepository;

  private List<RequisitionTemplate> requisitionTemplates;

  @Before
  public void setUp() {
    requisitionTemplates = new ArrayList<>();
  }

  RequisitionTemplateRepository getRepository() {
    return this.repository;
  }

  RequisitionTemplate generateInstance() {
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(
        new HashMap<>());
    requisitionTemplate.setNumberOfPeriodsToAverage(3);
    requisitionTemplate.setName("test-name" + getNextInstanceNumber());
    requisitionTemplate.setPopulateStockOnHandFromStockCards(false);
    return requisitionTemplate;
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrder() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn = new RequisitionTemplateColumn(
        "name", "label", "I", 1, true, SOURCE, getColumn(), null, null);
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
    AvailableRequisitionColumn availableRequisitionColumn = getColumn();
    availableRequisitionColumn.setCanChangeOrder(false);

    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
        new RequisitionTemplateColumn("testColumn1", "Test Column 1", "A", 1,
            false, SourceType.CALCULATED, availableRequisitionColumn, null, null);
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_KEY, 2);
    assertEquals(1, testColumn1.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateDisplayStatus() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
        new RequisitionTemplateColumn(
            "testColumn2", "Test Column 2", "B", 1, false,
            SourceType.CALCULATED, getColumn(), null, null);
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY, true);
    AvailableRequisitionColumn columnDefinition = testColumn1.getColumnDefinition();
    assertEquals(false, columnDefinition.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
    columnDefinition.setIsDisplayRequired(true);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY, false);
    assertEquals(true, columnDefinition.getIsDisplayRequired());
    assertEquals(true, testColumn1.getIsDisplayed());
  }

  @Test
  public void testChangeRequisitionTemplateLabel() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 =
        new RequisitionTemplateColumn(
            "testColumn3", "Test Column 3", "C", 1, false,
            SourceType.CALCULATED, getColumn(), null, null);
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    requisitionTemplate.changeColumnLabel(COLUMN_KEY, "newLabel");
    assertEquals(testColumn1.getLabel(), "newLabel");

  }

  @Test
  public void testChangeRequisitionTemplateSource() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column = new RequisitionTemplateColumn("column1", "label1", "I", 1,
        false, SourceType.CALCULATED, getColumn(), null, null);
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
  public void testChangeRequisitionTemplateOption() {
    AvailableRequisitionColumnOption option = getOption("34b8e763-71a0-41f1-86b4-1829963f0704");

    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column =
        new RequisitionTemplateColumn("column2", "label2", "I", 1,
            false, SourceType.CALCULATED, getColumnWithOption(), option, null);
    columns.put(COLUMN_KEY, column);

    RequisitionTemplate requisitionTemplate = generateInstance();
    requisitionTemplate.setColumnsMap(columns);
    requisitionTemplate = repository.save(requisitionTemplate);

    column = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(column.getOption(), option);
    assertEquals(column.getOption().getOptionName(), option.getOptionName());

    AvailableRequisitionColumnOption option2 = getOption("4957ebb4-297c-459e-a291-812e72286eff");
    requisitionTemplate.changeColumnOption(COLUMN_KEY, option2);
    requisitionTemplate = repository.save(requisitionTemplate);

    column = requisitionTemplate.getColumnsMap().get(COLUMN_KEY);
    assertEquals(column.getOption(), option2);
    assertEquals(column.getOption().getOptionName(), option2.getOptionName());
  }

  @Test
  public void testIsProductCodeFirstWhenDisplayed() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column =
        new RequisitionTemplateColumn("productCode", "label", "I", 2,
            false, SourceType.CALCULATED, getColumn(), null, null);
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
  public void testSearchRequisitionTemplatesByAllParameters() {
    for (int reqTemplateCount = 0; reqTemplateCount < 5; reqTemplateCount++) {
      RequisitionTemplate requisitionTemplate = generateInstance();
      requisitionTemplate.addAssignment(UUID.randomUUID(), null);
      requisitionTemplates.add(repository.save(requisitionTemplate));
    }

    UUID programId = requisitionTemplates
        .get(0)
        .getProgramId();

    RequisitionTemplate template = repository.getTemplateForProgram(programId);

    assertNotNull(template);
    assertThat(template.getProgramId(), is(programId));
  }

  @Test
  public void shouldFindLastTemplateByProgram() {
    // given
    UUID programId = UUID.randomUUID();
    Integer numberOfPeriodsToAverage = 5;
    ZonedDateTime createdDate = ZonedDateTime.now().minusMonths(1);

    for (int reqTemplateCount = 0; reqTemplateCount < 5; reqTemplateCount++) {
      createdDate = createdDate.plusDays(1);
      RequisitionTemplate requisitionTemplate = generateInstance();
      requisitionTemplate.addAssignment(programId, UUID.randomUUID());
      requisitionTemplate.setNumberOfPeriodsToAverage(numberOfPeriodsToAverage);
      requisitionTemplates.add(repository.save(requisitionTemplate));

      requisitionTemplate.setCreatedDate(createdDate);
      repository.save(requisitionTemplate);
    }

    // when
    RequisitionTemplate result = repository.getTemplateForProgram(programId);

    // then
    assertEquals(
        requisitionTemplates.get(requisitionTemplates.size() - 1).getId(), result.getId());
    assertThat(result.getProgramId(), is(programId));
    assertEquals(numberOfPeriodsToAverage, result.getNumberOfPeriodsToAverage());
  }

  private AvailableRequisitionColumn getColumn() {
    return availableRequisitionColumnRepository.findOne(
        UUID.fromString("4a2e9fd3-1127-4b68-9912-84a5c00f6999"));
  }

  private AvailableRequisitionColumn getColumnWithOption() {
    return availableRequisitionColumnRepository.findOne(
        UUID.fromString("5708ebf9-9317-4420-85aa-71b2ae92643d"));
  }

  private AvailableRequisitionColumnOption getOption(String uuid) {
    return availableRequisitionColumnOptionRepository.findOne(UUID.fromString(uuid));
  }
}
