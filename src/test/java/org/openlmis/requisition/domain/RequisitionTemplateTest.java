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

package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.openlmis.requisition.dto.RequisitionTemplateColumnDto;
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.testutils.RequisitionTemplateDataBuilder;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class RequisitionTemplateTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  private RequisitionTemplate requisitionTemplate;

  private static final String[] COLUMN_NAMES = {"column1", "column2", "column3", "column4"};

  @Before
  public void setUp() {
    requisitionTemplate = new RequisitionTemplate();
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setCanChangeOrder(true);
    RequisitionTemplateColumn column1 = new RequisitionTemplateColumn(columnDefinition);
    column1.setName(COLUMN_NAMES[0]);
    column1.setDisplayOrder(1);
    RequisitionTemplateColumn column2 = new RequisitionTemplateColumn(columnDefinition);
    column2.setName(COLUMN_NAMES[1]);
    column2.setDisplayOrder(2);
    RequisitionTemplateColumn column3 = new RequisitionTemplateColumn(columnDefinition);
    column3.setName(COLUMN_NAMES[2]);
    column3.setDisplayOrder(3);
    RequisitionTemplateColumn column4 = new RequisitionTemplateColumn(columnDefinition);
    column4.setName(COLUMN_NAMES[3]);
    column4.setDisplayOrder(4);
    Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<>();
    columnsMap.put(column1.getName(), column1);
    columnsMap.put(column2.getName(), column2);
    columnsMap.put(column3.getName(), column3);
    columnsMap.put(column4.getName(), column4);
    requisitionTemplate.setColumnsMap(columnsMap);
  }

  @Test
  public void testChangeColumnDisplayOrderToLower() {
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_NAMES[2], 1);
    Map<String, RequisitionTemplateColumn> mapAfterChange = requisitionTemplate.getColumnsMap();
    Assert.assertEquals(1, mapAfterChange.get(COLUMN_NAMES[2]).getDisplayOrder());
    Assert.assertEquals(2, mapAfterChange.get(COLUMN_NAMES[0]).getDisplayOrder());
    Assert.assertEquals(3, mapAfterChange.get(COLUMN_NAMES[1]).getDisplayOrder());
    Assert.assertEquals(4, mapAfterChange.get(COLUMN_NAMES[3]).getDisplayOrder());
  }

  @Test
  public void testChangeColumnDisplayOrderToHigher() {
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_NAMES[0], 3);
    Map<String, RequisitionTemplateColumn> mapAfterChange = requisitionTemplate.getColumnsMap();
    Assert.assertEquals(1, mapAfterChange.get(COLUMN_NAMES[1]).getDisplayOrder());
    Assert.assertEquals(2, mapAfterChange.get(COLUMN_NAMES[2]).getDisplayOrder());
    Assert.assertEquals(3, mapAfterChange.get(COLUMN_NAMES[0]).getDisplayOrder());
    Assert.assertEquals(4, mapAfterChange.get(COLUMN_NAMES[3]).getDisplayOrder());
  }

  @Test
  public void testChangeColumnDisplayOrderToTheSame() {
    requisitionTemplate.changeColumnDisplayOrder(COLUMN_NAMES[1], 2);
    Map<String, RequisitionTemplateColumn> mapAfterChange = requisitionTemplate.getColumnsMap();
    Assert.assertEquals(1, mapAfterChange.get(COLUMN_NAMES[0]).getDisplayOrder());
    Assert.assertEquals(2, mapAfterChange.get(COLUMN_NAMES[1]).getDisplayOrder());
    Assert.assertEquals(3, mapAfterChange.get(COLUMN_NAMES[2]).getDisplayOrder());
    Assert.assertEquals(4, mapAfterChange.get(COLUMN_NAMES[3]).getDisplayOrder());
  }

  @Test
  public void shouldCheckIfItHasColumnsDefined() {
    assertTrue(requisitionTemplate.hasColumnsDefined());
    assertFalse(new RequisitionTemplate(new HashMap<>()).hasColumnsDefined());
  }

  @Test
  public void shouldThrowIfSourceIsNotAvailableInColumn() {
    expectedException.expect(ValidationMessageException.class);
    requisitionTemplate.changeColumnSource(COLUMN_NAMES[0], SourceType.REFERENCE_DATA);

    expectedException.expectMessage(RequisitionTemplate.SOURCE + SourceType.REFERENCE_DATA
        + RequisitionTemplate.WARNING_SUFFIX);
  }

  @Test
  public void shouldThrowIfOptionIsNotAvailableInColumn() {
    expectedException.expect(ValidationMessageException.class);
    AvailableRequisitionColumnOption option = new AvailableRequisitionColumnOption(
        requisitionTemplate.getColumnsMap().get(COLUMN_NAMES[0])
            .getColumnDefinition(), "option1", "label1");
    requisitionTemplate.changeColumnOption(COLUMN_NAMES[0], option);

    expectedException.expectMessage(RequisitionTemplate.OPTION + option.getOptionName()
        + RequisitionTemplate.WARNING_SUFFIX);
  }

  /*@Test
  public void equalsContract() {
    EqualsVerifier
        .forClass(RequisitionTemplate.class)
        .withRedefinedSuperclass()
        .withPrefabValues(RequisitionTemplateColumn.class,
            new RequisitionTemplateColumnDataBuilder().build(),
            new RequisitionTemplateColumnDataBuilder().build())
        .verify();
  }*/

  @Test
  public void shouldExportRequisitionTemplate() {
    RequisitionTemplateDto templateDto = new RequisitionTemplateDto();
    requisitionTemplate.export(templateDto);

    assertEquals(requisitionTemplate.getId(), templateDto.getId());
    assertEquals(requisitionTemplate.getCreatedDate(), templateDto.getCreatedDate());
    assertEquals(requisitionTemplate.getModifiedDate(), templateDto.getModifiedDate());
    assertEquals(requisitionTemplate.getProgramId(), templateDto.getProgramId());
    assertEquals(requisitionTemplate.isPopulateStockOnHandFromStockCardsEnabled(),
        templateDto.isPopulateStockOnHandFromStockCardsEnabled());
    assertEquals(requisitionTemplate.getNumberOfPeriodsToAverage(),
        templateDto.getNumberOfPeriodsToAverage());
  }

  @Test
  public void shouldImportRequisitionTemplate() {
    RequisitionTemplateDto templateDto = new RequisitionTemplateDto();
    RequisitionTemplate template = new RequisitionTemplateDataBuilder()
        .withRequiredColumns()
        .build();
    template.export(templateDto);
    templateDto.setColumnsMap(template.getColumnsMap().entrySet().stream()
        .collect(Collectors.toMap(Map.Entry::getKey, entry -> {
          RequisitionTemplateColumnDto dto = new RequisitionTemplateColumnDto();
          entry.getValue().export(dto);
          return dto;
        })));

    RequisitionTemplate newTemplate = RequisitionTemplate.newInstance(templateDto);

    assertEquals(templateDto.getId(), newTemplate.getId());
    assertEquals(templateDto.getCreatedDate(), newTemplate.getCreatedDate());
    assertEquals(templateDto.getModifiedDate(), newTemplate.getModifiedDate());
    assertEquals(templateDto.getProgramId(), newTemplate.getProgramId());
    assertEquals(templateDto.isPopulateStockOnHandFromStockCardsEnabled(),
        newTemplate.isPopulateStockOnHandFromStockCardsEnabled());
    assertEquals(templateDto.getNumberOfPeriodsToAverage(),
        newTemplate.getNumberOfPeriodsToAverage());
    newTemplate.getColumnsMap().entrySet()
        .stream()
        .forEach(requisitionTemplateColumnEntry -> template.getColumnsMap()
            .get(requisitionTemplateColumnEntry.getKey())
            .equals(requisitionTemplateColumnEntry.getValue()));
    assertEquals(templateDto.getId(), newTemplate.getId());
  }
}
