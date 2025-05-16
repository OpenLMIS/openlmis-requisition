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

package org.openlmis.requisition.web;

import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import guru.nidi.ramltester.junit.RamlMatchers;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionLineItemDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.FacilityOperatorDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.dto.GeographicLevelDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.FacilityOperatorDtoDataBuilder;
import org.openlmis.requisition.testutils.FacilityTypeDtoDataBuilder;
import org.openlmis.requisition.testutils.GeographicLevelDtoDataBuilder;
import org.openlmis.requisition.testutils.GeographicZoneDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.ProcessingPeriodDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramOrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.RequisitionReportDtoDataBuilder;
import org.openlmis.requisition.testutils.SupportedProgramDtoDataBuilder;
import org.openlmis.requisition.testutils.UserDtoDataBuilder;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

@SuppressWarnings("PMD.TooManyMethods")
public class ReportsControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String PRINT_URL = "/api/requisitions/{id}/print";

  @Before
  public void setUp() {
    mockUserAuthenticated();
    doReturn(ValidationResult.success()).when(permissionService).canViewRequisition(anyUuid());
  }

  // GET /api/requisitions/{id}/print

  @Test
  public void shouldNotPrintRequisitionWhenDoesNotExist() {
    // given
    given(requisitionRepository.findById(anyUuid())).willReturn(Optional.empty());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldPrintRequisitionWhenExists() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    requisition.setTemplate(generateValidRequisitionTemplate());

    RequisitionReportDto reportDto = generateValidRequisitionReportDto();
    when(requisitionReportDtoBuilder.build(any(Requisition.class)))
        .thenReturn(reportDto);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(200);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private RequisitionTemplate generateValidRequisitionTemplate() {
    List<String> availableColumns = Arrays.asList(
        RequisitionLineItem.REQUESTED_QUANTITY,
        RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION,
        RequisitionLineItem.BEGINNING_BALANCE,
        RequisitionLineItem.TOTAL_RECEIVED_QUANTITY,
        RequisitionLineItem.STOCK_ON_HAND,
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY,
        RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS,
        RequisitionLineItem.APPROVED_QUANTITY,
        RequisitionLineItem.REMARKS_COLUMN,
        RequisitionLineItem.TOTAL_STOCKOUT_DAYS,
        RequisitionLineItem.TOTAL_COLUMN,
        RequisitionLineItem.NUMBER_OF_NEW_PATIENTS_ADDED,
        RequisitionLineItem.ADJUSTED_CONSUMPTION,
        RequisitionLineItem.AVERAGE_CONSUMPTION,
        RequisitionLineItem.MAXIMUM_STOCK_QUANTITY,
        RequisitionLineItem.CALCULATED_ORDER_QUANTITY
    );

    Map<String, RequisitionTemplateColumn> columns = availableColumns
        .stream()
        .collect(Collectors.toMap(
            Function.identity(),
            this::generateValidRequisitionTemplateColumn
        ));

    return new RequisitionTemplate(columns);
  }

  private RequisitionTemplateColumn generateValidRequisitionTemplateColumn(String name) {
    return new RequisitionTemplateColumnDataBuilder()
        .withName(name)
        .withLabel(name)
        .build();
  }

  private RequisitionReportDto generateValidRequisitionReportDto() {
    // Note: this does not need consistent data, we just need to make sure the data is present
    UUID programId = UUID.randomUUID();

    List<RequisitionLineItemDto> fullSupply = IntStream
        .range(1, 3)
        .mapToObj(idx -> generateValidRequisitionLineItemDto(programId))
            .collect(Collectors.toList());

    List<RequisitionLineItemDto> nonFullSupply = IntStream
        .range(1, 5)
        .mapToObj(idx -> generateValidRequisitionLineItemDto(programId))
            .collect(Collectors.toList());

    List<RequisitionLineItemDto> items = Stream
        .concat(fullSupply.stream(), nonFullSupply.stream())
        .collect(Collectors.toList());

    UserDto user = generateValidUserDto();
    RequisitionDto requisition = generateValidRequisitionDto(programId, items);

    return new RequisitionReportDtoDataBuilder()
        .withRequisition(requisition)
        .withFullSupply(fullSupply)
        .withNonFullSupply(nonFullSupply)
        .withFullSupplyTotalCost(Money.zero(CurrencyUnit.EUR))
        .withNonFullSupplyTotalCost(Money.zero(CurrencyUnit.EUR))
        .withTotalCost(Money.zero(CurrencyUnit.EUR))
        .withInitiatedBy(user)
        .withInitiatedDate(ZonedDateTime.now())
        .withSubmittedBy(user)
        .withSubmittedDate(ZonedDateTime.now())
        .withAuthorizedBy(user)
        .withAuthorizedDate(ZonedDateTime.now())
        .buildAsDto();
  }

  private UserDto generateValidUserDto() {
    return new UserDtoDataBuilder().buildAsDto();
  }

  private RequisitionDto generateValidRequisitionDto(
      UUID programId, List<RequisitionLineItemDto> items) {
    RequisitionDto requisition = new RequisitionDataBuilder()
        .withStatus(RequisitionStatus.AUTHORIZED)
        .withEmergency(false)
        .withSupplyingFacilityId(UUID.randomUUID())
        .withSupervisoryNodeId(UUID.randomUUID())
            .buildAsDto();
    requisition.setRequisitionLineItems(items);
    requisition.setFacility(generateValidFacilityDto(programId));
    requisition.setProgram(generateValidProgramDto(programId));
    requisition.setProcessingPeriod(generateValidProcessingPeriodDto());

    return requisition;
  }

  private ProcessingPeriodDto generateValidProcessingPeriodDto() {
    return new ProcessingPeriodDtoDataBuilder()
        .withDurationInMonths(5)
        .buildAsDto();
  }

  private ProgramDto generateValidProgramDto(UUID programId) {
    return new ProgramDtoDataBuilder()
        .withId(programId)
        .withActive(true)
        .withPeriodsSkippable(false)
        .withShowNonFullSupplyTab(true)
        .buildAsDto();
  }

  private RequisitionLineItemDto generateValidRequisitionLineItemDto(UUID programId) {
    RequisitionLineItemDto item = new RequisitionLineItemDataBuilder()
        .withRequisition(new RequisitionDataBuilder()
            .withProgramId(programId)
            .build())
        .buildAsDto();
    item.setOrderable(generateValidOrderableDto(programId));

    return item;
  }

  private FacilityDto generateValidFacilityDto(UUID... supportedPrograms) {
    FacilityTypeDto type = new FacilityTypeDtoDataBuilder()
        .withDisplayOrder(1)
        .withActive(true)
        .buildAsDto();
    GeographicLevelDto level = new GeographicLevelDtoDataBuilder().buildAsDto();
    GeographicZoneDto zone = new GeographicZoneDtoDataBuilder()
        .withLevel(level)
        .buildAsDto();
    FacilityOperatorDto operator = new FacilityOperatorDtoDataBuilder().buildAsDto();
    List<SupportedProgramDto> programs = Stream.of(supportedPrograms)
        .map(this::generateValidSupportedProgramDto)
            .collect(Collectors.toList());

    return new FacilityDtoDataBuilder()
        .withGoLiveDate(LocalDate.MIN)
        .withGoDownDate(LocalDate.MAX)
        .withEnabled(true)
        .withSupportedPrograms(programs)
        .withGeographicZone(zone)
        .withOperator(operator)
        .withType(type)
        .buildAsDto();
  }

  private SupportedProgramDto generateValidSupportedProgramDto(UUID id) {
    return new SupportedProgramDtoDataBuilder()
        .withId(id)
        .withProgramActive(true)
        .withPeriodsSkippable(false)
        .withShowNonFullSupplyTab(true)
        .withSupportActive(true)
        .withSupportStartDate(LocalDate.MAX)
        .buildAsDto();
  }

  private OrderableDto generateValidOrderableDto(UUID... programs) {
    Set<ProgramOrderableDto> products = Stream.of(programs)
        .map(this::generateValidProgramOrderableDto)
        .collect(Collectors.toSet());

    return new OrderableDtoDataBuilder()
        .withPrograms(products)
        .buildAsDto();
  }

  private ProgramOrderableDto generateValidProgramOrderableDto(UUID programId) {
    return new ProgramOrderableDtoDataBuilder()
        .withProgramId(programId)
        .buildAsDto();
  }
}
