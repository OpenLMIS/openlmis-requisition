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

package org.openlmis.requisition.service.report;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.text.NumberFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.RequisitionReportDtoDataBuilder;

public class RequisitionReportPayloadBuilderTest {

  private static final String DATE_FORMAT = "dd/MM/yyyy";
  private static final String SOH = "stockOnHand";
  private static final String SOH_LABEL = "Stock on hand";

  private final NumberFormat currencyFormat =
      NumberFormat.getCurrencyInstance(new Locale("en", "US"));

  /**
   * The report DTO reaches the builder after RequisitionDtoBuilder has resolved the facility,
   * program and period, so the fixture has to carry them too.
   */
  private RequisitionReportDto reportDto() {
    RequisitionDto requisition = new RequisitionDataBuilder().buildAsDto();
    requisition.setFacility(new FacilityDtoDataBuilder().buildAsDto());
    requisition.setProgram(DtoGenerator.of(ProgramDto.class));
    requisition.setProcessingPeriod(DtoGenerator.of(ProcessingPeriodDto.class));
    return new RequisitionReportDtoDataBuilder().withRequisition(requisition).buildAsDto();
  }

  @Test
  public void shouldFlattenRequisitionIntoPlainValues() {
    RequisitionReportDto reportDto = reportDto();

    Map<String, Object> record = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    assertEquals(reportDto.getRequisition().getFacility().getName(), record.get("facilityName"));
    assertEquals(reportDto.getRequisition().getFacility().getCode(), record.get("facilityCode"));
    assertEquals(reportDto.getRequisition().getProgram().getName(), record.get("programName"));
    assertEquals(reportDto.getRequisition().getStatus().toString(), record.get("status"));
    assertTrue(record.get("fullSupply") instanceof List);
    assertTrue(record.get("nonFullSupply") instanceof List);
  }

  @Test
  public void shouldProduceOnlyJsonSerializableValues() throws Exception {
    RequisitionReportDto reportDto = reportDto();

    Map<String, Object> record = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    // the record travels to the report service as JSON
    ObjectMapper mapper = new ObjectMapper();
    Map<?, ?> roundTripped = mapper.readValue(mapper.writeValueAsBytes(record), Map.class);

    assertEquals(record.get("facilityName"), roundTripped.get("facilityName"));
    assertEquals(record.get("status"), roundTripped.get("status"));
  }

  @Test
  public void shouldLeaveMissingUsersAndDatesNull() {
    RequisitionReportDto reportDto = reportDto();
    reportDto.setAuthorizedBy(null);
    reportDto.setAuthorizedDate(null);

    Map<String, Object> record = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    assertNull(record.get("authorizedBy"));
    assertNull(record.get("authorizedDate"));
  }

  @Test
  public void shouldKeyColumnsThatStillUseTheShippedLabel() {
    AvailableRequisitionColumn definition = new AvailableRequisitionColumn();
    definition.setName(SOH);
    definition.setLabel(SOH_LABEL);

    RequisitionTemplateColumn untouched = new RequisitionTemplateColumnDataBuilder()
        .withName(SOH)
        .withLabel(SOH_LABEL)
        .withColumnDefinition(definition)
        .build();

    Map<String, RequisitionTemplateColumn> columns = new LinkedHashMap<>();
    columns.put(SOH, untouched);

    assertEquals("report.column." + SOH,
        RequisitionReportPayloadBuilder.buildColumnLabelKeys(columns).get(SOH));
    assertEquals(SOH_LABEL,
        RequisitionReportPayloadBuilder.buildColumnLabels(columns).get(SOH));
  }

  @Test
  public void shouldNotKeyColumnsAnAdministratorRenamed() {
    AvailableRequisitionColumn definition = new AvailableRequisitionColumn();
    definition.setName(SOH);
    definition.setLabel(SOH_LABEL);

    RequisitionTemplateColumn renamed = new RequisitionTemplateColumnDataBuilder()
        .withName(SOH)
        .withLabel("Close bal")
        .withColumnDefinition(definition)
        .build();

    Map<String, RequisitionTemplateColumn> columns = new LinkedHashMap<>();
    columns.put(SOH, renamed);

    // a renamed label is printed as entered, in every language
    assertNull(RequisitionReportPayloadBuilder.buildColumnLabelKeys(columns).get(SOH));
    assertEquals("Close bal",
        RequisitionReportPayloadBuilder.buildColumnLabels(columns).get(SOH));
  }
}
