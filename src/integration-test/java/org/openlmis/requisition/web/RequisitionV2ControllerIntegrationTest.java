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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DATE_MODIFIED_MISMATCH;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.web.BaseRequisitionController.IDEMPOTENCY_KEY_HEADER;
import static org.openlmis.requisition.web.ResourceNames.FACILITIES;
import static org.openlmis.requisition.web.ResourceNames.PROCESSING_PERIODS;
import static org.openlmis.requisition.web.ResourceNames.PROGRAMS;

import guru.nidi.ramltester.junit.RamlMatchers;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.requisition.ApprovedProductReference;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.RequisitionValidationService;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.domain.requisition.VersionEntityReference;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ObjectReferenceDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionLineItemV2Dto;
import org.openlmis.requisition.dto.RequisitionV2Dto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.service.referencedata.ApproveProductsAggregator;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings({"PMD.TooManyMethods"})
public class RequisitionV2ControllerIntegrationTest extends BaseRequisitionWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/v2/requisitions";
  private static final String INITIATE_URL = RESOURCE_URL + "/initiate";
  private static final String ID_URL = RESOURCE_URL + "/{id}";

  private static final String FACILITY = "facility";
  private static final String PROGRAM = "program";
  private static final String SUGGESTED_PERIOD = "suggestedPeriod";
  private static final String EMERGENCY = "emergency";
  private static final String MESSAGE_KEY = "messageKey";

  @Autowired
  private RequisitionV2Controller controller;

  private List<StockAdjustmentReason> stockAdjustmentReasons;

  private UUID idempotencyKey = UUID.randomUUID();

  @Before
  public void setUp() {
    super.setUp();

    mockUserAuthenticated();

    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();
    mockStockEventServiceResponses();

    stockAdjustmentReasons = mockReasons();

    mockSearchSupervisoryNodeByProgramAndFacility();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(false);

    ReflectionTestUtils.setField(controller, BaseRequisitionController.class,
        "baseUrl", BASE_URL, String.class);
  }

  // POST /api/v2/requisitions/initiate

  @Test
  public void shouldInitiateRequisition() {
    // given
    ProgramDto program = mockProgram();
    FacilityDto facility = mockFacility();
    ProcessingPeriodDto period = mockPeriod();
    Requisition requisition = generateRequisition(
        RequisitionStatus.INITIATED, program.getId(), facility.getId());
    requisition.setProcessingPeriodId(period.getId());

    generateApprovedProducts(requisition);

    doReturn(ValidationResult.success())
        .when(permissionService)
        .canInitRequisition(program.getId(), facility.getId());
    doReturn(requisition)
        .when(requisitionService)
        .initiate(eq(program), eq(facility), eq(period), eq(false),
            anyListOf(StockAdjustmentReason.class), eq(requisition.getTemplate()),
            any(ApproveProductsAggregator.class));
    mockValidationSuccess();

    // when
    RequisitionV2Dto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(HttpStatus.CREATED.value())
        .extract()
        .as(RequisitionV2Dto.class);

    // then
    assertEquals(requisition.getId(), result.getId());
    verify(facilityReferenceDataService).findOne(facility.getId());
    verify(validReasonStockmanagementService).search(program.getId(), facility.getType().getId());

    verify(reasonsValidator).validate(stockAdjustmentReasons, requisition.getTemplate());
    verify(requisitionService, atLeastOnce())
        .initiate(eq(program), eq(facility), eq(period), eq(false),
            eq(stockAdjustmentReasons), eq(requisition.getTemplate()),
            any(ApproveProductsAggregator.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionIfRequestIsInvalid() {
    // given
    UUID programId = mockProgram().getId();
    FacilityDto facilityDto = mockFacility();
    doReturn(ValidationResult.success())
        .when(permissionService)
        .canInitRequisition(programId, facilityDto.getId());
    mockFacilityDoesNotSupportProgram(facilityDto, programId);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityDto.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(HttpStatus.BAD_REQUEST.value())
        .body(MESSAGE_KEY, is(ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionForUnauthorizedRequest() {
    restAssured
        .given()
        .queryParam(PROGRAM, UUID.randomUUID())
        .queryParam(FACILITY, UUID.randomUUID())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(HttpStatus.UNAUTHORIZED.value());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenUserHasNoRight() {
    // given
    UUID programId = mockProgram().getId();
    UUID facilityId = mockFacility().getId();

    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, REQUISITION_CREATE))
        .when(permissionService)
        .canInitRequisition(programId, facilityId);

    // then
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityId)
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(HttpStatus.FORBIDDEN.value())
        .body(MESSAGE_KEY, is(PERMISSION_ERROR_MESSAGE));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionIfThereIsConflict() {
    // given
    ProgramDto program = mockProgram();
    FacilityDto facility = mockFacility();
    ProcessingPeriodDto period = mockPeriod();
    Requisition requisition =
        generateRequisition(RequisitionStatus.INITIATED, program.getId(), facility.getId());
    requisition.setProcessingPeriodId(period.getId());

    doReturn(ValidationResult.success())
        .when(permissionService)
        .canInitRequisition(program.getId(), facility.getId());
    doReturn(requisition)
        .when(requisitionService)
        .initiate(eq(program), eq(facility), eq(period), eq(false),
            anyListOf(StockAdjustmentReason.class), eq(requisition.getTemplate()),
            any(ApproveProductsAggregator.class));
    mockValidationSuccess();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, idempotencyKey)
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(HttpStatus.CONFLICT.value())
        .body(MESSAGE_KEY, is(IDEMPOTENCY_KEY_ALREADY_USED));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/v2/requisitions/{id}

  @Test
  public void shouldGetRequisition() {
    // given
    mockFacility();
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    doReturn(ValidationResult.success())
        .when(permissionService)
        .canViewRequisition(requisition);

    generateApprovedProducts(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(HttpStatus.OK.value())
        .header(HttpHeaders.ETAG, "W/1")
        .body("id", is(requisition.getId().toString()));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetRequisitionForUnauthorizedRequest() {
    restAssured.given()
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(HttpStatus.UNAUTHORIZED.value());
  }

  @Test
  public void shouldNotGetRequisitionWhenUserHasNoRight() {
    // given
    doReturn(Optional.of(mock(Requisition.class))).when(requisitionRepository).findById(anyUuid());
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, REQUISITION_AUTHORIZE))
        .when(permissionService)
        .canViewRequisition(any(Requisition.class));

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(HttpStatus.FORBIDDEN.value())
        .body(MESSAGE_KEY, is(PERMISSION_ERROR_MESSAGE));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetRequisitionIfItDoesNotExist() {
    // given
    UUID testRequisitionId = UUID.randomUUID();
    given(requisitionRepository.findById(testRequisitionId)).willReturn(Optional.empty());
    doReturn(ValidationResult.success())
        .when(permissionService)
        .canViewRequisition(any(Requisition.class));

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", testRequisitionId)
        .when()
        .get(ID_URL)
        .then()
        .statusCode(HttpStatus.NOT_FOUND.value())
        .body(MESSAGE_KEY, is(ERROR_REQUISITION_NOT_FOUND));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/v2/requisitions/{id}

  @Test
  public void shouldUpdateRequisition() {
    // given
    ProcessingPeriodDto period = mockPeriod();
    Requisition requisition = generateRequisition();
    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    mockValidationSuccess();

    RequisitionV2Dto requisitionDto = generateRequisitionDto(requisition);

    doReturn(Optional.of(requisition))
        .when(requisitionRepository)
        .findById(requisition.getId());

    doReturn(ValidationResult.success())
        .when(permissionService)
        .canUpdateRequisition(any(Requisition.class));

    when(requisitionService
        .validateCanSaveRequisition(requisition))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator
        .validateEtagVersionIfPresent(any(HttpServletRequest.class), any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator
        .validateRequisitionTimestamps(any(ZonedDateTime.class), any(Requisition.class)))
        .thenReturn(ValidationResult.success());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionDto.getId())
        .when()
        .body(requisitionDto)
        .put(ID_URL)
        .then()
        .statusCode(HttpStatus.OK.value())
        .extract()
        .as(RequisitionV2Dto.class);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionIfRequestIsInvalid() {
    // given
    mockPeriod();
    Requisition requisition = spy(generateRequisition());

    UUID requisitionId = requisition.getId();
    doReturn(ValidationResult.success())
        .when(permissionService)
        .canUpdateRequisition(any(Requisition.class));
    when(requisitionService
        .validateCanSaveRequisition(requisition))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator
        .validateEtagVersionIfPresent(any(HttpServletRequest.class), any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator
        .validateRequisitionTimestamps(any(ZonedDateTime.class), any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    doReturn(ValidationResult.fieldErrors(
        Collections.singletonMap(REQUISITION_LINE_ITEMS, new Message(ERROR_INCORRECT_VALUE))))
        .when(requisition).validateCanBeUpdated(any(RequisitionValidationService.class));
    when(requisitionRepository.findById(requisitionId)).thenReturn(Optional.of(requisition));

    RequisitionV2Dto requisitionDto = generateRequisitionDto(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .body(requisitionDto)
        .put(ID_URL)
        .then()
        .statusCode(HttpStatus.BAD_REQUEST.value())
        .body(REQUISITION_LINE_ITEMS + '.' + MESSAGE_KEY, is(ERROR_INCORRECT_VALUE));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionForUnauthorizedRequest() {
    // given
    Requisition requisition = generateRequisition();
    RequisitionV2Dto requisitionDto = generateRequisitionDto(requisition);

    // expect
    restAssured.given()
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionDto.getId())
        .when()
        .body(requisitionDto)
        .put(ID_URL)
        .then()
        .statusCode(HttpStatus.UNAUTHORIZED.value());
  }

  @Test
  public void shouldNotUpdateRequisitionWhenUserHasNoRight() {
    // given
    Requisition requisition = generateRequisition();
    RequisitionV2Dto requisitionDto = generateRequisitionDto(requisition);

    when(requisitionService
        .validateCanSaveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, REQUISITION_CREATE));
    when(requisitionVersionValidator
        .validateRequisitionTimestamps(any(ZonedDateTime.class), any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    doReturn(Optional.of(mock(Requisition.class))).when(requisitionRepository).findById(anyUuid());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionDto.getId())
        .when()
        .body(requisitionDto)
        .put(ID_URL)
        .then()
        .statusCode(HttpStatus.FORBIDDEN.value())
        .body(MESSAGE_KEY, is(PERMISSION_ERROR_MESSAGE));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionIfItDoesNotExist() {
    // given
    Requisition requisition = generateRequisition();
    RequisitionV2Dto requisitionDto = generateRequisitionDto(requisition);

    when(requisitionService
        .validateCanSaveRequisition(requisition))
        .thenReturn(ValidationResult.success());
    given(requisitionRepository.findById(requisition.getId())).willReturn(Optional.empty());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionDto.getId())
        .when()
        .body(requisitionDto)
        .put(ID_URL)
        .then()
        .statusCode(HttpStatus.NOT_FOUND.value())
        .body(MESSAGE_KEY, is(ERROR_REQUISITION_NOT_FOUND));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionIfThereIsConflict() {
    // given
    ProgramDto program = mockProgram();
    FacilityDto facility = mockFacility();
    ProcessingPeriodDto period = mockPeriod();
    Requisition requisition =
        generateRequisition(RequisitionStatus.INITIATED, program.getId(), facility.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    when(requisitionService
        .validateCanSaveRequisition(requisition))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator
        .validateEtagVersionIfPresent(any(HttpServletRequest.class), any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator
        .validateRequisitionTimestamps(any(ZonedDateTime.class), any(Requisition.class)))
        .thenCallRealMethod();

    mockValidationSuccess();

    RequisitionV2Dto requisitionDto = generateRequisitionDto(requisition);
    requisitionDto.setModifiedDate(requisition.getModifiedDate().minusDays(4));

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .body(requisitionDto)
        .put(ID_URL)
        .then()
        .statusCode(HttpStatus.CONFLICT.value())
        .body(MESSAGE_KEY, is(ERROR_DATE_MODIFIED_MISMATCH));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private RequisitionV2Dto generateRequisitionDto(Requisition requisition) {
    RequisitionV2Dto requisitionDto = new RequisitionV2Dto();
    requisition.export(requisitionDto);

    requisitionDto
        .setFacility(new ObjectReferenceDto(requisition.getFacilityId(), null, FACILITIES));
    requisitionDto
        .setProcessingPeriod(new ObjectReferenceDto(requisition.getProcessingPeriodId(),
            null, PROCESSING_PERIODS));
    requisitionDto
        .setProgram(new ObjectReferenceDto(requisition.getProgramId(), null, PROGRAMS));

    List<ApprovedProductDto> approvedProducts = generateApprovedProducts(requisition);

    requisitionDto.setRequisitionLineItems(requisition
        .getRequisitionLineItems()
        .stream()
        .map(line -> {
          OrderableDto orderableDto = findOrderable(approvedProducts, line);
          ApprovedProductDto approvedProductDto = findApprovedProduct(approvedProducts, line);

          RequisitionLineItemV2Dto lineDto = new RequisitionLineItemV2Dto();
          line.export(lineDto, orderableDto, approvedProductDto);

          return lineDto;
        })
        .collect(Collectors.toList()));

    return requisitionDto;
  }

  private List<ApprovedProductDto> generateApprovedProducts(Requisition requisition) {
    List<ApprovedProductDto> approvedProducts = requisition
        .getRequisitionLineItems()
        .stream()
        .map(line -> new ApprovedProductDtoDataBuilder()
            .withId(line.getFacilityTypeApprovedProduct().getId())
            .withVersionNumber(line.getFacilityTypeApprovedProduct().getVersionNumber())
            .withOrderable(new OrderableDtoDataBuilder()
                .withId(line.getOrderable().getId())
                .withVersionNumber(line.getOrderable().getVersionNumber())
                .withProgramOrderable(requisition.getProgramId(), true)
                .buildAsDto())
            .buildAsDto())
        .collect(Collectors.toList());

    int index = 0;
    for (ApprovedProductReference product : requisition.getAvailableProducts()) {
      approvedProducts.add(new ApprovedProductDtoDataBuilder()
          .withId(product.getFacilityTypeApprovedProduct().getId())
          .withVersionNumber(product.getFacilityTypeApprovedProduct().getVersionNumber())
          .withOrderable(new OrderableDtoDataBuilder()
              .withId(product.getOrderable().getId())
              .withVersionNumber(product.getOrderable().getVersionNumber())
              .withProgramOrderable(requisition.getProgramId(), index % 2 == 0)
              .buildAsDto())
          .buildAsDto());

      ++index;
    }

    List<OrderableDto> orderables = approvedProducts
        .stream()
        .map(ApprovedProductDto::getOrderable)
        .collect(Collectors.toList());

    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(orderables);

    given(facilityTypeApprovedProductReferenceDataService
        .findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(approvedProducts);

    when(approvedProductReferenceDataService.getApprovedProducts(anyUuid(), anyUuid()))
        .thenAnswer(invocation -> new ApproveProductsAggregator(
            approvedProducts, invocation.getArgument(1, UUID.class)));

    return approvedProducts;
  }

  private ApprovedProductDto findApprovedProduct(List<ApprovedProductDto> approvedProducts,
      RequisitionLineItem line) {
    return approvedProducts
        .stream()
        .filter(product -> product.getIdentity()
            .equals(new VersionIdentityDto(line.getFacilityTypeApprovedProduct())))
        .findFirst()
        .orElse(null);
  }

  private OrderableDto findOrderable(List<ApprovedProductDto> approvedProducts,
      RequisitionLineItem line) {
    return approvedProducts
        .stream()
        .map(ApprovedProductDto::getOrderable)
        .filter(orderable -> orderable.getIdentity()
            .equals(new VersionIdentityDto(line.getOrderable())))
        .findFirst()
        .orElse(null);
  }

}
