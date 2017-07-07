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
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Mockito.when;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.DispensableDto;
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
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.springframework.boot.test.mock.mockito.MockBean;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@SuppressWarnings("PMD.TooManyMethods")
public class ReportsControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String PRINT_URL = "/api/requisitions/{id}/print";

  @MockBean
  private RequisitionReportDtoBuilder requisitionReportDtoBuilder;

  @MockBean
  private RequisitionRepository requisitionRepository;

  @MockBean
  private OrderableReferenceDataService orderableReferenceDataService;

  @Before
  public void setUp() {
    mockUserAuthenticated();
    given(orderableReferenceDataService.findByIds(anySetOf(UUID.class))).willReturn(
            Collections.emptyList());
  }

  // GET /api/requisitions/{id}/print

  @Test
  public void shouldNotPrintRequisitionWhenDoesNotExist() {
    // given
    given(requisitionRepository.findOne(anyUuid())).willReturn(null);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(APPLICATION_JSON)
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
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(APPLICATION_JSON)
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

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(columns);
    return template;
  }

  private RequisitionTemplateColumn generateValidRequisitionTemplateColumn(String name) {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();

    column.setName(name);
    column.setLabel(name);
    column.setIsDisplayed(true);

    return column;
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

    RequisitionReportDto reportDto = new RequisitionReportDto();

    reportDto.setRequisition(requisition);
    reportDto.setFullSupply(fullSupply);
    reportDto.setNonFullSupply(nonFullSupply);
    reportDto.setFullSupplyTotalCost(Money.zero(CurrencyUnit.EUR));
    reportDto.setNonFullSupplyTotalCost(Money.zero(CurrencyUnit.EUR));
    reportDto.setTotalCost(Money.zero(CurrencyUnit.EUR));
    reportDto.setInitiatedBy(user);
    reportDto.setInitiatedDate(ZonedDateTime.now());
    reportDto.setSubmittedBy(user);
    reportDto.setSubmittedDate(ZonedDateTime.now());
    reportDto.setAuthorizedBy(user);
    reportDto.setAuthorizedDate(ZonedDateTime.now());

    return reportDto;
  }

  private UserDto generateValidUserDto() {
    UserDto user = new UserDto();

    user.setId(UUID.randomUUID());
    user.setUsername("username");
    user.setFirstName("john");
    user.setLastName("doe");
    user.setEmail("johndoe@example.com");
    user.setVerified(true);
    user.setActive(true);

    return user;
  }

  private RequisitionDto generateValidRequisitionDto(
      UUID programId, List<RequisitionLineItemDto> items) {
    RequisitionDto requisition = new RequisitionDto();

    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setModifiedDate(ZonedDateTime.now());
    requisition.setRequisitionLineItems(items);
    requisition.setDraftStatusMessage("message");
    requisition.setFacility(generateValidFacilityDto(programId));
    requisition.setProgram(generateValidProgramDto(programId));
    requisition.setProcessingPeriod(generateValidProcessingPeriodDto());
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setEmergency(false);
    requisition.setSupplyingFacility(UUID.randomUUID());
    requisition.setSupervisoryNode(UUID.randomUUID());
    requisition.setStatusChanges(Collections.emptyList());

    return requisition;
  }

  private ProcessingPeriodDto generateValidProcessingPeriodDto() {
    ProcessingPeriodDto period = new ProcessingPeriodDto();

    period.setId(UUID.randomUUID());
    period.setName("requisitionName");
    period.setDescription("requisitionDesc");
    period.setDurationInMonths(5);

    return period;
  }

  private ProgramDto generateValidProgramDto(UUID programId) {
    ProgramDto program = new ProgramDto();

    program.setId(programId);
    program.setCode("programCode");
    program.setName("programName");
    program.setDescription("programDesc");
    program.setActive(true);
    program.setPeriodsSkippable(false);
    program.setShowNonFullSupplyTab(true);

    return program;
  }

  private RequisitionLineItemDto generateValidRequisitionLineItemDto(UUID programId) {
    RequisitionLineItemDto item = new RequisitionLineItemDto();

    item.setOrderable(generateValidOrderableDto(programId));
    item.setBeginningBalance(10);
    item.setTotalReceivedQuantity(5);
    item.setTotalLossesAndAdjustments(2);
    item.setStockOnHand(5);
    item.setRequestedQuantity(10);
    item.setTotalConsumedQuantity(5);
    item.setRequestedQuantityExplanation("explanation");
    item.setRemarks("remarks");
    item.setApprovedQuantity(50);
    item.setTotalStockoutDays(5);
    item.setTotal(100);
    item.setPacksToShip(5L);
    item.setPricePerPack(Money.zero(CurrencyUnit.EUR));
    item.setNumberOfNewPatientsAdded(5);
    item.setTotalCost(Money.zero(CurrencyUnit.EUR));
    item.setSkipped(false);
    item.setAdjustedConsumption(50);
    item.setPreviousAdjustedConsumptions(Collections.emptyList());
    item.setAverageConsumption(5);
    item.setMaxPeriodsOfStock(BigDecimal.valueOf(10));
    item.setMaximumStockQuantity(5);
    item.setCalculatedOrderQuantity(30);

    return item;
  }

  private FacilityDto generateValidFacilityDto(UUID... supportedPrograms) {
    GeographicLevelDto level = new GeographicLevelDto();
    level.setCode("zoneCode");
    level.setName("zoneName");
    level.setLevelNumber(1);

    GeographicZoneDto zone = new GeographicZoneDto();
    zone.setCode("zoneCode");
    zone.setName("zoneName");
    zone.setLevel(level);

    FacilityOperatorDto operator = new FacilityOperatorDto();
    operator.setCode("operatorCode");
    operator.setName("operatorName");

    FacilityTypeDto type = new FacilityTypeDto();
    type.setCode("typeCode");
    type.setName("typeName");
    type.setDescription("typeDesc");
    type.setDisplayOrder(1);
    type.setActive(true);

    FacilityDto facility = new FacilityDto();
    List<SupportedProgramDto> programs = Stream.of(supportedPrograms)
        .map(this::generateValidSupportedProgramDto)
        .collect(Collectors.toList());

    facility.setDescription("description");
    facility.setActive(true);
    facility.setGoLiveDate(LocalDate.MIN);
    facility.setGoDownDate(LocalDate.MAX);
    facility.setComment("comment");
    facility.setEnabled(true);
    facility.setOpenLmisAccessible(true);
    facility.setSupportedPrograms(programs);
    facility.setGeographicZone(zone);
    facility.setOperator(operator);
    facility.setType(type);

    return facility;
  }

  private SupportedProgramDto generateValidSupportedProgramDto(UUID id) {
    SupportedProgramDto program = new SupportedProgramDto();

    program.setId(id);
    program.setCode("supportedCode");
    program.setName("supportedName");
    program.setDescription("supportedDesc");
    program.setProgramActive(true);
    program.setPeriodsSkippable(false);
    program.setShowNonFullSupplyTab(true);
    program.setSupportActive(true);
    program.setSupportStartDate(LocalDate.MAX);

    return program;
  }

  private OrderableDto generateValidOrderableDto(UUID... programs) {
    Set<ProgramOrderableDto> products = Stream.of(programs)
        .map(this::generateValidProgramOrderableDto)
        .collect(Collectors.toSet());

    OrderableDto orderable = new OrderableDto();

    orderable.setId(UUID.randomUUID());
    orderable.setProductCode("productCode");
    orderable.setFullProductName("fullName");
    orderable.setPrograms(products);
    orderable.setDispensable(new DispensableDto("unit"));

    return orderable;
  }

  private ProgramOrderableDto generateValidProgramOrderableDto(UUID programId) {
    ProgramOrderableDto product = new ProgramOrderableDto();

    product.setProgramId(UUID.randomUUID());
    product.setOrderableDisplayCategoryId(UUID.randomUUID());
    product.setOrderableCategoryDisplayName("categoryName");
    product.setPricePerPack(Money.zero(CurrencyUnit.EUR));

    return product;
  }
}
