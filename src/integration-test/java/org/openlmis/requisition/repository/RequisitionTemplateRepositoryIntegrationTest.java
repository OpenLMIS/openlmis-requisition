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

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.SourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;


/**
 * Allow testing requisitionTemplateRepository.
 */
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTemplateRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionTemplate> {

  private static final String COLUMN_KEY = "columnKey";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  @Autowired
  private RequisitionTemplateRepository repository;

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
    return new RequisitionTemplateDataBuilder()
        .withoutId()
        .withNumberOfPeriodsToAverage(3)
        .build();
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrder() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn = new RequisitionTemplateColumnDataBuilder()
        .withSource(SOURCE)
        .withColumnDefinition(getColumn())
        .withOption(null)
        .withTag(null)
        .build();
    columns.put(COLUMN_KEY, testColumn);
    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(1, testColumn.getDisplayOrder());
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_KEY, 2);
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(2, testColumn.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateColumnOrderWithCanChangeOrderFalse() {
    AvailableRequisitionColumn availableRequisitionColumn = getColumn();
    availableRequisitionColumn.setCanChangeOrder(false);

    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 = new RequisitionTemplateColumnDataBuilder()
        .withSource(SourceType.CALCULATED)
        .withColumnDefinition(availableRequisitionColumn)
        .withDisplay(false)
        .withOption(null)
        .withTag(null)
        .build();
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);
    testColumn1 = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_KEY, 2);
    assertEquals(1, testColumn1.getDisplayOrder());
  }

  @Test
  public void testChangeRequisitionTemplateDisplayStatus() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn testColumn1 = new RequisitionTemplateColumnDataBuilder()
        .withSource(SourceType.CALCULATED)
        .withColumnDefinition(getColumn())
        .withDisplay(false)
        .withOption(null)
        .withTag(null)
        .build();
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY, true);
    testColumn1 = requisitionTemplate.viewColumns().get(COLUMN_KEY);
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
    RequisitionTemplateColumn testColumn1 = new RequisitionTemplateColumnDataBuilder()
        .withSource(SourceType.CALCULATED)
        .withColumnDefinition(getColumn())
        .withDisplay(false)
        .withOption(null)
        .withTag(null)
        .build();
    columns.put(COLUMN_KEY, testColumn1);
    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);
    requisitionTemplate.changeColumnLabel(COLUMN_KEY, "newLabel");
    testColumn1 = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(testColumn1.getLabel(), "newLabel");

  }

  @Test
  public void testChangeRequisitionTemplateSource() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withSource(SourceType.CALCULATED)
        .withColumnDefinition(getColumn())
        .withDisplay(false)
        .withOption(null)
        .withTag(null)
        .build();
    columns.put(COLUMN_KEY, column);
    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);

    column = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(column.getSource(), SOURCE);

    requisitionTemplate.changeColumnSource(COLUMN_KEY, SourceType.USER_INPUT);
    requisitionTemplate = repository.save(requisitionTemplate);

    column = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(column.getSource(), SourceType.USER_INPUT);
  }

  @Test
  public void testChangeRequisitionTemplateOption() {
    AvailableRequisitionColumnOption option = getOption("34b8e763-71a0-41f1-86b4-1829963f0704");

    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withSource(SourceType.CALCULATED)
        .withColumnDefinition(getColumnWithOption())
        .withDisplay(false)
        .withOption(option)
        .withTag(null)
        .build();
    columns.put(COLUMN_KEY, column);

    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);

    column = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(column.getOption(), option);
    assertEquals(column.getOption().getOptionName(), option.getOptionName());

    AvailableRequisitionColumnOption option2 = getOption("4957ebb4-297c-459e-a291-812e72286eff");
    requisitionTemplate.changeColumnOption(COLUMN_KEY, option2);
    requisitionTemplate = repository.save(requisitionTemplate);

    column = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(column.getOption(), option2);
    assertEquals(column.getOption().getOptionName(), option2.getOptionName());
  }

  @Test
  public void testIsProductCodeFirstWhenDisplayed() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName("productCode")
        .withLabel("label")
        .withIndicator("I")
        .withDisplayOrder(2)
        .withDisplay(false)
        .withSource(SourceType.CALCULATED)
        .withColumnDefinition(getColumn())
        .withOption(null)
        .withDefinition(null)
        .withTag(null)
        .build();

    columns.put(COLUMN_KEY, column);
    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(column.getDisplayOrder(), 2);
    requisitionTemplate.changeColumnDisplay(COLUMN_KEY, true);
    requisitionTemplate = repository.save(requisitionTemplate);
    column = requisitionTemplate.viewColumns().get(COLUMN_KEY);
    assertEquals(column.getDisplayOrder(), 1);
  }

  @Test
  public void testSearchRequisitionTemplatesByAllParameters() {
    for (int reqTemplateCount = 0; reqTemplateCount < 5; reqTemplateCount++) {
      RequisitionTemplate requisitionTemplate = generateInstance();
      requisitionTemplate.addAssignment(UUID.randomUUID(), UUID.randomUUID(),
          new Random().nextBoolean());
      requisitionTemplates.add(repository.save(requisitionTemplate));
    }

    UUID programId = requisitionTemplates
        .get(0)
        .getProgramId();
    UUID facilityTypeId = requisitionTemplates
        .get(0)
        .getFacilityTypeIds()
        .iterator()
        .next();
    Boolean requisitionReportOnly = requisitionTemplates
        .get(0).getRequisitionReportOnly();

    RequisitionTemplate template = repository.findTemplate(programId, facilityTypeId,
        requisitionReportOnly);

    assertNotNull(template);
    assertThat(template.getProgramId(), is(programId));
  }

  @Test
  public void shouldAllowDuplicationForArchivedTemplateNames() {
    RequisitionTemplate template = generateInstance();
    template.archive();
    repository.saveAndFlush(template);

    RequisitionTemplate newTemplate = new RequisitionTemplateDataBuilder()
        .build();
    newTemplate.updateFrom(template);

    repository.saveAndFlush(newTemplate);
  }

  @Test(expected = DataIntegrityViolationException.class)
  public void shouldNotAllowDuplicationForNonArchivedTemplateNames() {
    RequisitionTemplate template = generateInstance();
    repository.saveAndFlush(template);

    RequisitionTemplate newTemplate = new RequisitionTemplateDataBuilder()
        .build();
    newTemplate.updateFrom(template);

    repository.saveAndFlush(newTemplate);
  }

  @Test(expected = DataIntegrityViolationException.class)
  public void shouldNotAllowDuplicationForNonArchivedTemplateNamesCamelCase() {
    RequisitionTemplate template = generateInstance();
    repository.saveAndFlush(template);

    RequisitionTemplate newTemplate = new RequisitionTemplateDataBuilder()
        .withNumberOfPeriodsToAverage(3)
        .withPopulateStockOnHandFromStockCards(true)
        .withName(StringUtils.swapCase(template.getName()))
        .buildAsNew();

    repository.saveAndFlush(newTemplate);
  }

  @Test(expected = DataIntegrityViolationException.class)
  public void shouldNotAllowToHaveTwoTemplatesWithSameProgramAndFacilityType() {
    UUID programId = UUID.randomUUID();
    UUID facilityTypeId = UUID.randomUUID();
    Boolean requisitionReportOnly = new Random().nextBoolean();

    RequisitionTemplate template = generateInstance();
    template.addAssignment(programId, facilityTypeId, requisitionReportOnly);
    repository.saveAndFlush(template);

    template = generateInstance();
    template.addAssignment(programId, facilityTypeId, requisitionReportOnly);
    repository.saveAndFlush(template);
  }

  @Test
  public void shouldGetActiveTemplates() {
    int size = 6; // make sure to have an even number here
    List<UUID> currentTemplates = new ArrayList<>();

    for (int i = 0; i < size; ++i) {
      RequisitionTemplate requisitionTemplate = generateInstance();
      requisitionTemplate.addAssignment(UUID.randomUUID(), null,
          false);

      if (i % 2 == 0) {
        requisitionTemplate.archive();
      }

      requisitionTemplates.add(repository.save(requisitionTemplate));

      if (!requisitionTemplate.isArchived()) {
        currentTemplates.add(requisitionTemplate.getId());
      }
    }

    assertThat(currentTemplates, hasSize(size / 2));

    List<RequisitionTemplate> templates = repository.getActiveTemplates();
    List<UUID> templateIds = templates
        .stream()
        .map(BaseEntity::getId)
        .sorted()
        .collect(Collectors.toList());

    Collections.sort(currentTemplates);

    assertThat(templateIds, hasSize(3));
    assertThat(templateIds, contains(currentTemplates.toArray(new UUID[0])));
  }

  private AvailableRequisitionColumn getColumn() {
    return availableRequisitionColumnRepository.findById(
        UUID.fromString("4a2e9fd3-1127-4b68-9912-84a5c00f6999")).orElse(null);
  }

  private AvailableRequisitionColumn getColumnWithOption() {
    return availableRequisitionColumnRepository.findById(
        UUID.fromString("5708ebf9-9317-4420-85aa-71b2ae92643d")).orElse(null);
  }

  private AvailableRequisitionColumnOption getOption(String uuid) {
    return availableRequisitionColumnOptionRepository.findById(UUID.fromString(uuid)).orElse(null);
  }

  @Test
  public void shouldAllowDuplicationForRequisitionReportOnlyTemplateNames() {
    RequisitionTemplate template = generateInstance();
    template.requisitionReportingOnly();
    repository.saveAndFlush(template);
  }

  @Test(expected = DataIntegrityViolationException.class)
  public void shouldNotAllowToHaveTwoTemplatesWithSameProgramFacilityTypeAndReportOnly() {
    UUID programId = UUID.randomUUID();
    UUID facilityTypeId = UUID.randomUUID();

    RequisitionTemplate template = generateInstance();
    template.addAssignment(programId, facilityTypeId, true);
    repository.saveAndFlush(template);

    template = generateInstance();
    template.addAssignment(programId, facilityTypeId, true);
    repository.saveAndFlush(template);
  }

  @Test
  public void shouldAllowDuplicationForReportingOnlyTemplateNames() {
    RequisitionTemplate template = generateInstance();
    template.requisitionReportingOnly();
    repository.saveAndFlush(template);

    RequisitionTemplate newTemplate = new RequisitionTemplateDataBuilder()
        .build();
    newTemplate.updateFrom(template);

    repository.saveAndFlush(newTemplate);
  }
}
