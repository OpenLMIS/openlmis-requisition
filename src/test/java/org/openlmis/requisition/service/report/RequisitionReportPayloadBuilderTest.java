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
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Test;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.dto.DispensableDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.RequisitionReportDtoDataBuilder;

public class RequisitionReportPayloadBuilderTest {

  private static final String DATE_FORMAT = "dd/MM/yyyy";
  private static final String FULL_SUPPLY = "fullSupply";

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

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    assertEquals(reportDto.getRequisition().getFacility().getName(),
        reportRecord.get("facilityName"));
    assertEquals(reportDto.getRequisition().getFacility().getCode(),
        reportRecord.get("facilityCode"));
    assertEquals(reportDto.getRequisition().getProgram().getName(),
        reportRecord.get("programName"));
    assertEquals(reportDto.getRequisition().getStatus().toString(), reportRecord.get("status"));
    assertTrue(reportRecord.get(FULL_SUPPLY) instanceof List);
    assertTrue(reportRecord.get("nonFullSupply") instanceof List);
  }

  @Test
  public void shouldProduceOnlyJsonSerializableValues() throws Exception {
    RequisitionReportDto reportDto = reportDto();

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    // the reportRecord travels to the report service as JSON
    ObjectMapper mapper = new ObjectMapper();
    Map<?, ?> roundTripped = mapper.readValue(mapper.writeValueAsBytes(reportRecord), Map.class);

    assertEquals(reportRecord.get("facilityName"), roundTripped.get("facilityName"));
    assertEquals(reportRecord.get("status"), roundTripped.get("status"));
  }

  @Test
  public void shouldLeaveMissingUsersAndDatesNull() {
    RequisitionReportDto reportDto = reportDto();
    reportDto.setAuthorizedBy(null);
    reportDto.setAuthorizedDate(null);

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    assertNull(reportRecord.get("authorizedBy"));
    assertNull(reportRecord.get("authorizedDate"));
  }

  @Test
  public void shouldFlattenLineItemsSoTheTemplateReadsPlainKeys() {
    RequisitionDto requisition = new RequisitionDataBuilder().buildAsDto();
    requisition.setFacility(new FacilityDtoDataBuilder().buildAsDto());
    requisition.setProgram(DtoGenerator.of(ProgramDto.class));
    requisition.setProcessingPeriod(DtoGenerator.of(ProcessingPeriodDto.class));

    OrderableDto orderable = new OrderableDto();
    orderable.setProductCode("C100");
    orderable.setFullProductName("Levora");
    orderable.setNetContent(10);
    orderable.setDispensable(new DispensableDto("pack", "each"));
    orderable.setPrograms(Collections.singleton(new ProgramOrderableDto(
        requisition.getProgram().getId(), null, "Category A", 1, true, 1, 1,
        Money.of(CurrencyUnit.USD, 2))));

    RequisitionLineItemDto lineItem = new RequisitionLineItemDto();
    lineItem.setOrderable(orderable);
    lineItem.setBeginningBalance(100);
    lineItem.setPricePerPack(Money.of(CurrencyUnit.USD, 2));
    lineItem.setTotalCost(Money.of(CurrencyUnit.USD, 20));

    RequisitionReportDto reportDto = new RequisitionReportDtoDataBuilder()
        .withRequisition(requisition)
        .withFullSupply(Collections.singletonList(lineItem))
        .buildAsDto();

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);
    List<?> fullSupply = (List<?>) reportRecord.get(FULL_SUPPLY);
    Map<?, ?> flattened = (Map<?, ?>) fullSupply.get(0);

    assertEquals("C100", flattened.get("productCode"));
    assertEquals("Levora", flattened.get("fullProductName"));
    assertEquals("each", flattened.get("dispensableDisplayUnit"));
    assertEquals("Category A", flattened.get("categoryDisplayName"));
    assertEquals(10L, flattened.get("netContent"));
    assertEquals(100, flattened.get("beginningBalance"));
    // money is formatted here because the formatter cannot be sent to the report service
    assertEquals(currencyFormat.format(20), flattened.get("totalCost"));
  }

  @Test
  public void shouldReturnEmptyListWhenThereAreNoLineItems() {
    RequisitionReportDto reportDto = reportDto();

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    assertTrue(((List<?>) reportRecord.get(FULL_SUPPLY)).isEmpty());
    assertTrue(((List<?>) reportRecord.get("nonFullSupply")).isEmpty());
  }

  @Test
  public void shouldHandleLineItemsWithoutOrderableOrMoney() {
    RequisitionDto requisition = new RequisitionDataBuilder().buildAsDto();
    requisition.setFacility(new FacilityDtoDataBuilder().buildAsDto());
    requisition.setProgram(DtoGenerator.of(ProgramDto.class));
    requisition.setProcessingPeriod(DtoGenerator.of(ProcessingPeriodDto.class));

    RequisitionLineItemDto lineItem = new RequisitionLineItemDto();
    lineItem.setBeginningBalance(5);

    RequisitionReportDto reportDto = new RequisitionReportDtoDataBuilder()
        .withRequisition(requisition)
        .withFullSupply(Collections.singletonList(lineItem))
        .withTotalCost(null)
        .buildAsDto();

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);
    Map<?, ?> flattened = (Map<?, ?>) ((List<?>) reportRecord.get(FULL_SUPPLY)).get(0);

    assertNull(reportRecord.get("totalCost"));
    assertNull(flattened.get("productCode"));
    assertNull(flattened.get("dispensableDisplayUnit"));
    assertEquals(5, flattened.get("beginningBalance"));
  }

  @Test
  public void shouldTreatNullLineItemListAsEmpty() {
    RequisitionDto requisition = new RequisitionDataBuilder().buildAsDto();
    requisition.setFacility(new FacilityDtoDataBuilder().buildAsDto());
    requisition.setProgram(DtoGenerator.of(ProgramDto.class));
    requisition.setProcessingPeriod(DtoGenerator.of(ProcessingPeriodDto.class));

    RequisitionReportDto reportDto = new RequisitionReportDtoDataBuilder()
        .withRequisition(requisition)
        .withFullSupply(null)
        .buildAsDto();

    Map<String, Object> reportRecord = RequisitionReportPayloadBuilder
        .buildReportRecord(reportDto, DATE_FORMAT, currencyFormat);

    assertTrue(((List<?>) reportRecord.get(FULL_SUPPLY)).isEmpty());
  }
}
