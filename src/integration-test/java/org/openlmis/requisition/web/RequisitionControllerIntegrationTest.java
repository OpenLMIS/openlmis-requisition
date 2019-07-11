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

import static java.time.format.DateTimeFormatter.ISO_DATE;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.anyList;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DUPLICATE_STATUS_CHANGE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_END_DATE_WRONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PROGRAM_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SERVICE_REQUIRED;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_DELETE;
import static org.openlmis.requisition.web.BaseRequisitionController.IDEMPOTENCY_KEY_HEADER;

import com.google.common.collect.Lists;
import guru.nidi.ramltester.junit.RamlMatchers;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.PersistenceException;
import javax.servlet.http.HttpServletRequest;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.RequisitionValidationService;
import org.openlmis.requisition.domain.requisition.StatusMessage;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.domain.requisition.VersionEntityReference;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.ReasonType;
import org.openlmis.requisition.dto.ReleasableRequisitionDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.ValidReasonDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.repository.custom.ProcessedRequestsRedisRepository;
import org.openlmis.requisition.service.DataRetrievalException;
import org.openlmis.requisition.service.PageDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.ValidReasonStockmanagementService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.testutils.ReasonDtoDataBuilder;
import org.openlmis.requisition.testutils.ReleasableRequisitionDtoDataBuilder;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.validate.ReasonsValidator;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.postgresql.util.PSQLException;
import org.postgresql.util.ServerErrorMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.orm.jpa.JpaSystemException;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class RequisitionControllerIntegrationTest extends BaseRequisitionWebIntegrationTest {

  private static final String SIZE = "size";
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String INITIATE_URL = RESOURCE_URL + "/initiate";
  private static final String APPROVE_URL = RESOURCE_URL + "/{id}/approve";
  private static final String SKIP_URL = RESOURCE_URL + "/{id}/skip";
  private static final String REJECT_URL = RESOURCE_URL + "/{id}/reject";
  private static final String SUBMIT_URL = RESOURCE_URL + "/{id}/submit";
  private static final String SUBMITTED_URL = RESOURCE_URL + "/submitted";
  private static final String AUTHORIZATION_URL = RESOURCE_URL + "/{id}/authorize";
  private static final String CONVERT_TO_ORDER_URL = RESOURCE_URL + "/convertToOrder";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String REQ_FOR_APPROVAL_URL = RESOURCE_URL + "/requisitionsForApproval";
  private static final String PERIODS_FOR_INITIATE_URL = RESOURCE_URL + "/periodsForInitiate";
  private static final String APPROVED_REQUISITIONS_SEARCH_URL = RESOURCE_URL
      + "/requisitionsForConvert";

  private static final String FACILITY = "facility";
  private static final String PROGRAM = "program";
  private static final String FACILITY_ID = "facilityId";
  private static final String PROGRAM_ID = "programId";
  private static final String SUGGESTED_PERIOD = "suggestedPeriod";
  private static final String EMERGENCY = "emergency";
  private static final String MESSAGE = "message";
  private static final String REQUISITION_STATUS = "requisitionStatus";
  private static final String SUPERVISORY_NODE = "supervisoryNode";
  private static final String PROCESSING_PERIOD = "processingPeriod";
  private static final String INITIATED_DATE_FROM = "initiatedDateFrom";
  private static final String INITIATED_DATE_TO = "initiatedDateTo";
  private static final String FACILITY_CODE_ASC = "facilityCode,asc";
  private static final String MODIFIED_DATE_FROM = "modifiedDateFrom";
  private static final String MODIFIED_DATE_TO = "modifiedDateTo";

  @MockBean
  private StatusMessageRepository statusMessageRepository;

  @MockBean
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @MockBean
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @MockBean
  private DatePhysicalStockCountCompletedEnabledPredicate predicate;

  @MockBean
  private PeriodService periodService;

  @MockBean
  private RequisitionService requisitionService;

  @MockBean
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @MockBean
  private OrderableReferenceDataService orderableReferenceDataService;

  @MockBean(name = "facilityReferenceDataService")
  private FacilityReferenceDataService facilityReferenceDataService;

  @MockBean
  private ValidReasonStockmanagementService validReasonStockmanagementService;

  @MockBean
  private ReasonsValidator reasonsValidator;

  @MockBean
  private ProcessedRequestsRedisRepository processedRequestsRedisRepository;

  @MockBean
  private RequisitionVersionValidator requisitionVersionValidator;

  @Autowired
  private MessageService messageService;

  @Autowired
  private DateHelper dateHelper;

  @Autowired
  private RequisitionController requisitionController;

  private List<StockAdjustmentReason> stockAdjustmentReasons;
  private UUID facilityTypeId = UUID.randomUUID();
  private UUID key = UUID.randomUUID();
  private String wrongFormatKey = "some-key";
  private UserDto user;
  private UUID facilityId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();

  @Before
  public void setUp() {
    super.setUp();

    user = mockUserAuthenticated();

    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();
    mockStockEventServiceResponses();

    mockReasons();

    mockSearchSupervisoryNodeByProgramAndFacility();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(false);
    ReflectionTestUtils.setField(requisitionController, BaseRequisitionController.class,
        "baseUrl", BASE_URL, String.class);
  }

  @Test
  public void shouldGetChosenRequisition() {
    // given
    mockFacility();
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    doReturn(ValidationResult.success())
        .when(permissionService).canViewRequisition(requisition);

    // when
    RequisitionDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .header(HttpHeaders.ETAG, "W/1")
        .extract()
        .as(RequisitionDto.class);

    // then
    assertEquals(requisition.getId(), result.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetChosenRequisitionWhenUserHasNoRightForView() {
    // given
    String missingPermission = REQUISITION_AUTHORIZE;
    doReturn(mock(Requisition.class)).when(requisitionRepository).findOne(anyUuid());
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, missingPermission))
        .when(permissionService).canViewRequisition(any(Requisition.class));

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonExistentRequisition() {
    // given
    doReturn(ValidationResult.success()).when(permissionService).canViewRequisition(anyUuid());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnBadRequestWhenUpdateRequisitionValidationFailure() {
    // given
    Requisition requisition = spy(generateRequisition());

    UUID requisitionId = requisition.getId();
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
    when(requisitionRepository.findOne(requisitionId)).thenReturn(requisition);

    // when
    RequisitionDto requisitionDto = DtoGenerator.of(RequisitionDto.class);
    requisitionDto.setId(requisitionId);
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .body(requisitionDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(400)
        .body(REQUISITION_LINE_ITEMS + ".messageKey", is(ERROR_INCORRECT_VALUE))
        .body(REQUISITION_LINE_ITEMS + ".message", notNullValue());

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteRequisition() {
    // given
    Requisition requisition = generateRequisition();
    doReturn(ValidationResult.success())
        .when(permissionService).canDeleteRequisition(requisition);
    doNothing().when(requisitionService).delete(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(204);

    // then
    verify(requisitionService, atLeastOnce()).delete(requisition);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionWhenUserHasNoRightForDelete() {
    // given
    String missingPermission = REQUISITION_DELETE;
    Requisition requisition = generateRequisition();
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, missingPermission))
        .when(permissionService).canDeleteRequisition(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonExistentRequisition() {
    // given
    UUID requisitionId = UUID.randomUUID();
    given(requisitionRepository.findOne(requisitionId)).willReturn(null);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionWithWrongStatus() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    doReturn(ValidationResult.success())
        .when(permissionService).canDeleteRequisition(requisition);

    String errorKey = MessageKeys.ERROR_DELETE_FAILED_WRONG_STATUS;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(requisitionService).delete(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/search

  @Test
  public void shouldFindRequisitionsByAllParameters() {
    // given
    UUID periodId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID facilityId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();
    LocalDate dateTo = LocalDate.now().plusDays(10);
    LocalDate dateFrom = LocalDate.now().minusDays(10);
    ZonedDateTime dateTimeFrom = ZonedDateTime.now().plusDays(10);
    ZonedDateTime dateTimeTo = ZonedDateTime.now().minusDays(10);

    MultiValueMap<String, String> queryMap = new LinkedMultiValueMap<>();
    queryMap.add(PROGRAM, programId.toString());
    queryMap.add(PROCESSING_PERIOD, periodId.toString());
    queryMap.add(FACILITY, facilityId.toString());
    queryMap.add(SUPERVISORY_NODE, supervisoryNodeId.toString());
    queryMap.add(REQUISITION_STATUS, RequisitionStatus.INITIATED.toString());
    queryMap.add(INITIATED_DATE_FROM, dateFrom.toString());
    queryMap.add(INITIATED_DATE_TO, dateTo.toString());
    queryMap.add(MODIFIED_DATE_FROM, dateTimeFrom.toString());
    queryMap.add(MODIFIED_DATE_TO, dateTimeTo.toString());
    queryMap.add(EMERGENCY, Boolean.FALSE.toString());

    QueryRequisitionSearchParams params = new QueryRequisitionSearchParams(queryMap);

    Requisition requisition = generateRequisition();

    given(requisitionService.searchRequisitions(eq(params), any(Pageable.class)))
        .willReturn(Pagination.getPage(singletonList(requisition), FIRST_PAGE));

    // when
    PageDto resultPage = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, programId)
        .queryParam(PROCESSING_PERIOD, periodId)
        .queryParam(FACILITY, facilityId)
        .queryParam(SUPERVISORY_NODE, supervisoryNodeId)
        .queryParam(REQUISITION_STATUS, RequisitionStatus.INITIATED)
        .queryParam(INITIATED_DATE_FROM, dateFrom.toString())
        .queryParam(INITIATED_DATE_TO, dateTo.toString())
        .queryParam(MODIFIED_DATE_FROM, dateTimeFrom.toString())
        .queryParam(MODIFIED_DATE_TO, dateTimeTo.toString())
        .queryParam(EMERGENCY, Boolean.FALSE.toString())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, resultPage.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldFindRequisitionsByMultipleStatuses() {
    // given
    RequisitionStatus submittedStatus = RequisitionStatus.SUBMITTED;
    RequisitionStatus authorizedStatus = RequisitionStatus.AUTHORIZED;

    MultiValueMap<String, String> queryMap = new LinkedMultiValueMap<>();
    queryMap.add(REQUISITION_STATUS, submittedStatus.toString());
    queryMap.add(REQUISITION_STATUS, authorizedStatus.toString());
    QueryRequisitionSearchParams params = new QueryRequisitionSearchParams(queryMap);

    List<Requisition> requisitions = generateRequisitions(submittedStatus, authorizedStatus);

    given(requisitionService
        .searchRequisitions(eq(params), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, FIRST_PAGE));

    // when
    PageDto resultPage = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(REQUISITION_STATUS, submittedStatus)
        .queryParam(REQUISITION_STATUS, authorizedStatus)
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(2, resultPage.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/{id}/submit

  @Test
  public void shouldSubmitValidRequisition() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);
    doReturn(new ProgramDtoDataBuilder().buildWithNotSkippedAuthorizationStep())
        .when(programReferenceDataService).findOne(anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();
    mockPeriod();

    // when
    BasicRequisitionDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(200)
        .extract().as(BasicRequisitionDto.class);

    // then
    assertEquals(requisition.getId(), result.getId());
    verify(requisition, atLeastOnce()).submit(any(), any(UUID.class), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldSubmitRequisitionWithIdempotencyKey() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);
    doReturn(new ProgramDtoDataBuilder().buildWithNotSkippedAuthorizationStep())
        .when(programReferenceDataService).findOne(anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();
    mockPeriod();

    BasicRequisitionDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(200)
        .header(HttpHeaders.LOCATION, BASE_URL + RESOURCE_URL + '/' + requisition.getId())
        .extract().as(BasicRequisitionDto.class);

    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, null);
    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, requisition.getId());

    assertEquals(requisition.getId(), result.getId());
    verify(requisition, atLeastOnce()).submit(any(), any(UUID.class), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithUsedIdempotencyKey() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);
    doReturn(new ProgramDtoDataBuilder().buildWithNotSkippedAuthorizationStep())
        .when(programReferenceDataService).findOne(anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(409)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithIdempotencyKeyInWrongFormat() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);
    doReturn(new ProgramDtoDataBuilder().buildWithNotSkippedAuthorizationStep())
        .when(programReferenceDataService).findOne(anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, wrongFormatKey)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_WRONG_FORMAT, wrongFormatKey)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitWhenPeriodEndDateIsInFuture() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);

    mockExternalServiceCalls();
    mockValidationSuccess();
    ProcessingPeriodDto period = mockPeriod(dateHelper.getCurrentDateWithSystemZone().plusDays(2));

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, is(getMessage(ERROR_PERIOD_END_DATE_WRONG, period.getEndDate())));

    // then
    verify(requisition, never()).submit(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid(), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWhenUserHasNoRightForSubmit() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    String missingPermission = REQUISITION_CREATE;
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, missingPermission))
        .when(permissionService).canSubmitRequisition(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisition, never()).submit(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid(), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAllowForStatusChangeDuplicationOnSubmit() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);
    doReturn(new ProgramDtoDataBuilder().buildWithNotSkippedAuthorizationStep())
        .when(programReferenceDataService).findOne(anyUuid());

    // specific psql format
    // C<<numbers>> -> sql state
    // M<<string>> -> short error message
    // \u0000 -> use for splitting parts of message
    // there are more fields but we need only those two for tests
    PSQLException psqlException = new PSQLException(
        new ServerErrorMessage(
            "C23505\u0000MERROR: Duplicate status change: SUBMITTED at supervisory node: <NULL>"
        )
    );

    doThrow(new JpaSystemException((RuntimeException) new PersistenceException(psqlException)))
        .when(requisitionRepository)
        .save(any(Requisition.class));

    mockExternalServiceCalls();
    mockValidationSuccess();
    mockPeriod();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, is(getMessage(ERROR_DUPLICATE_STATUS_CHANGE)));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/skip

  @Test
  public void shouldSkipRequisition() {
    // given
    Requisition requisition = generateRequisition();

    doReturn(ValidationResult.success())
        .when(permissionService).canUpdateRequisition(requisition);
    mockValidationSuccess();

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(200);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldSkipRequisitionWithIdempotencyKey() {
    Requisition requisition = generateRequisition();
    doReturn(ValidationResult.success())
        .when(permissionService).canUpdateRequisition(requisition);
    mockValidationSuccess();
    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .header(HttpHeaders.LOCATION, BASE_URL + RESOURCE_URL + '/' + requisition.getId())
        .statusCode(200);

    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, null);
    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, requisition.getId());

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionWithUsedIdempotencyKey() {
    Requisition requisition = generateRequisition();
    doReturn(ValidationResult.success())
        .when(permissionService).canUpdateRequisition(requisition);
    mockValidationSuccess();
    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(409)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionWithIdempotencyKeyInWrongFormat() {
    Requisition requisition = generateRequisition();
    doReturn(ValidationResult.success())
        .when(permissionService).canUpdateRequisition(requisition);
    mockValidationSuccess();
    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, wrongFormatKey)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_WRONG_FORMAT, wrongFormatKey)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfRequisitionNotExist() {
    // given
    UUID requisitionId = UUID.randomUUID();
    when(requisitionRepository.findOne(requisitionId)).thenReturn(null);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(404)
        .body(MESSAGE, equalTo(getMessage(ERROR_REQUISITION_NOT_FOUND, requisitionId)));

    // then
    verify(requisitionRepository, never()).save(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfUserHasNoRightForCreate() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);

    String missingPermission = REQUISITION_CREATE;

    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, missingPermission))
        .when(permissionService).canUpdateRequisition(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfProgramNotExist() {
    // given
    Requisition requisition = generateRequisition();

    doReturn(ValidationResult.success())
        .when(permissionService).canUpdateRequisition(requisition);
    mockValidationSuccess();

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);
    given(programReferenceDataService.findOne(requisition.getProgramId())).willReturn(null);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(404)
        .body(MESSAGE, equalTo(getMessage(ERROR_PROGRAM_NOT_FOUND, requisition.getProgramId())));

    // then
    verify(requisitionRepository, never()).save(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/reject

  @Test
  public void shouldRejectRequisition() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    given(requisitionService.reject(requisition, emptyMap())).willReturn(requisition);
    doReturn(ValidationResult.success())
        .when(permissionService).canApproveRequisition(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(200);

    // then
    verify(requisitionService, atLeastOnce()).reject(requisition, emptyMap());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldRejectRequisitionWithIdempotencyKey() {
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    given(requisitionService.reject(requisition, emptyMap())).willReturn(requisition);
    doReturn(ValidationResult.success())
        .when(permissionService).canApproveRequisition(requisition);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .header(HttpHeaders.LOCATION, BASE_URL + RESOURCE_URL + '/' + requisition.getId())
        .statusCode(200);

    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, null);
    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, requisition.getId());

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectRequisitionWithUsedIdempotencyKey() {
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    given(requisitionService.reject(requisition, emptyMap())).willReturn(requisition);
    doReturn(ValidationResult.success())
        .when(permissionService).canApproveRequisition(requisition);

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(409)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectRequisitionWithIdempotencyKeyInWrongFormat() {
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    given(requisitionService.reject(requisition, emptyMap())).willReturn(requisition);
    doReturn(ValidationResult.success())
        .when(permissionService).canApproveRequisition(requisition);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, wrongFormatKey)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_WRONG_FORMAT, wrongFormatKey)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectRequisitionWhenUserHasNoRightForApprove() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);

    String missingPermission = REQUISITION_APPROVE;
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, missingPermission))
        .when(permissionService).canApproveRequisition(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisitionService, never()).reject(requisition, emptyMap());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectRequisitionWithWrongStatus() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

    doReturn(ValidationResult.success())
        .when(permissionService).canApproveRequisition(requisition);

    String errorKey = MessageKeys.ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL;

    ValidationMessageException exception = mockValidationException(errorKey, requisitionId);
    doThrow(exception).when(requisitionService).reject(requisition, emptyMap());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey, requisitionId)));

    // then
    verify(requisitionRepository, never()).save(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/authorize

  @Test
  public void shouldAuthorizeRequisition() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    when(requisition.isApprovable()).thenReturn(true);

    List<OrderableDto> orderables = requisition
        .getRequisitionLineItems()
        .stream()
        .map(line -> new OrderableDtoDataBuilder()
            .withId(line.getOrderable().getId())
            .withVersionId(line.getOrderable().getVersionId())
            .withProgramOrderable(requisition.getProgramId(), true)
            .buildAsDto())
        .collect(Collectors.toList());

    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(orderables);
    doNothing().when(requisition).authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();
    mockPeriod();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(200);

    // then
    verify(requisition, atLeastOnce())
        .authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class), anyUuid());
    verify(supervisoryNodeReferenceDataService)
        .findSupervisoryNode(requisition.getProgramId(), requisition.getFacilityId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldAuthorizeRequisitionWithIdempotencyKey() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    when(requisition.isApprovable()).thenReturn(true);
    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();
    mockPeriod();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .header(HttpHeaders.LOCATION, BASE_URL + RESOURCE_URL + '/' + requisition.getId())
        .statusCode(200);

    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, null);
    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, requisition.getId());

    verify(requisition, atLeastOnce())
        .authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class), anyUuid());
    verify(supervisoryNodeReferenceDataService)
        .findSupervisoryNode(requisition.getProgramId(), requisition.getFacilityId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeRequisitionWithUsedIdempotencyKey() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    when(requisition.isApprovable()).thenReturn(true);
    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(409)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeRequisitionWithIdempotencyKeyInWrongFormat() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    when(requisition.isApprovable()).thenReturn(true);
    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, wrongFormatKey)
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_WRONG_FORMAT, wrongFormatKey)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeRequisitionWhenPeriodEndDateIsInFuture() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();
    ProcessingPeriodDto period = mockPeriod(dateHelper.getCurrentDateWithSystemZone().plusDays(2));

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, is(getMessage(ERROR_PERIOD_END_DATE_WRONG, period.getEndDate())));

    // then
    verify(requisition, never()).submit(anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyUuid(), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeRequisitionWhenUserHasNoRightForAuthorize() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    UUID requisitionId = requisition.getId();

    String missingPermission = REQUISITION_AUTHORIZE;
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, REQUISITION_AUTHORIZE))
        .when(permissionService).canAuthorizeRequisition(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisitionId)
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisition, never())
        .authorize(anyMapOf(VersionIdentityDto.class, OrderableDto.class), any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/{id}/initiate
  @Test
  public void shouldInitiateRequisition() {
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
            anyListOf(StockAdjustmentReason.class), eq(requisition.getTemplate()));
    mockValidationSuccess();

    // when
    RequisitionDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, period.getId())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(201)
        .extract().as(RequisitionDto.class);

    // then
    assertEquals(requisition.getId(), result.getId());
    verify(facilityReferenceDataService).findOne(facility.getId());
    verify(validReasonStockmanagementService).search(program.getId(), facilityTypeId);

    verify(reasonsValidator).validate(stockAdjustmentReasons, requisition.getTemplate());
    verify(requisitionService, atLeastOnce())
        .initiate(program, facility, period, false,
            stockAdjustmentReasons, requisition.getTemplate());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenNoStockService() {
    // given
    doReturn(ValidationResult.success())
        .when(permissionService).canInitRequisition(anyUuid(), anyUuid());
    DataRetrievalException exception =
        mockDataException(ERROR_SERVICE_REQUIRED, "Stock Management");
    doThrow(exception).when(validReasonStockmanagementService).search(anyUuid(), anyUuid());
    mockValidationSuccess();
    mockPeriod();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, mockProgram().getId())
        .queryParam(FACILITY, mockFacility().getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(500)
        .body(MESSAGE, equalTo(getMessage(ERROR_SERVICE_REQUIRED, "Stock Management")));

    // then
    verify(requisitionService, never()).initiate(
        any(ProgramDto.class), any(FacilityDto.class),
        any(ProcessingPeriodDto.class), anyBoolean(), any(), any()
    );
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.requestChecks());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenPeriodDoesNotExist() {
    // given
    ProgramDto program = mockProgram();
    FacilityDto facility = mockFacility();
    ProcessingPeriodDto period = mockPeriod();

    ValidationMessageException err =
        mockValidationException(MessageKeys.ERROR_INCORRECT_SUGGESTED_PERIOD);
    given(requisitionService
        .initiate(eq(program), eq(facility), eq(period), eq(false),
            anyListOf(StockAdjustmentReason.class), any(RequisitionTemplate.class)))
        .willThrow(err);

    doReturn(ValidationResult.success())
        .when(permissionService).canInitRequisition(program.getId(), facility.getId());
    mockValidationSuccess();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, program)
        .queryParam(FACILITY, facility)
        .queryParam(SUGGESTED_PERIOD, period.getId())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenFacilityDoesNotSupportProgram() {
    // given
    UUID programId = mockProgram().getId();
    FacilityDto facilityDto = mockFacility();
    UUID facilityId = facilityDto.getId();
    doReturn(ValidationResult.success())
        .when(permissionService).canInitRequisition(programId, facilityId);
    mockFacilityDoesNotSupportProgram(facilityDto, programId);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityId)
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM)));

    // then
    verify(requisitionService, never())
        .initiate(any(ProgramDto.class), any(FacilityDto.class),
            any(ProcessingPeriodDto.class), anyBoolean(), anyList(),
            any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenUserHasNoRight() {
    // given
    UUID programId = mockProgram().getId();
    UUID facilityId = mockFacility().getId();

    String missingPermission = REQUISITION_CREATE;
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, REQUISITION_CREATE))
        .when(permissionService).canInitRequisition(programId, facilityId);

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
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisitionService, never())
        .initiate(any(ProgramDto.class), any(FacilityDto.class),
            any(ProcessingPeriodDto.class), anyBoolean(), anyList(),
            any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldInitiateRequisitionWithIdempotencyKey() {
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
            anyListOf(StockAdjustmentReason.class), eq(requisition.getTemplate()));
    mockValidationSuccess();

    // when
    RequisitionDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(201)
        .header(HttpHeaders.LOCATION, BASE_URL + RESOURCE_URL + '/' + requisition.getId())
        .extract().as(RequisitionDto.class);

    // then
    assertEquals(requisition.getId(), result.getId());
    verify(facilityReferenceDataService).findOne(facility.getId());
    verify(validReasonStockmanagementService).search(program.getId(), facilityTypeId);

    verify(reasonsValidator).validate(stockAdjustmentReasons, requisition.getTemplate());
    verify(requisitionService, atLeastOnce())
        .initiate(program, facility, period, false,
            stockAdjustmentReasons, requisition.getTemplate());

    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, null);
    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, requisition.getId());

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWithUsedIdempotencyKey() {
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
            anyListOf(StockAdjustmentReason.class), eq(requisition.getTemplate()));
    mockValidationSuccess();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(409)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED)));

    // then
    verify(requisitionService, never())
        .initiate(any(ProgramDto.class), any(FacilityDto.class),
            any(ProcessingPeriodDto.class), anyBoolean(), anyList(),
            any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWithIdempotencyKeyInWrongFormat() {
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
            anyListOf(StockAdjustmentReason.class), eq(requisition.getTemplate()));
    mockValidationSuccess();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, wrongFormatKey)
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_WRONG_FORMAT, wrongFormatKey)));

    // then
    verify(requisitionService, never())
        .initiate(any(ProgramDto.class), any(FacilityDto.class),
            any(ProcessingPeriodDto.class), anyBoolean(), anyList(),
            any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldApproveRequisitionWithIdempotencyKey() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);

    doReturn(ValidationResult.success())
        .when(requisitionService).validateCanApproveRequisition(any(Requisition.class),
        anyUuid());
    doNothing().when(requisition).approve(anyUuid(),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();
    mockPeriod();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .header(HttpHeaders.LOCATION, BASE_URL + RESOURCE_URL + '/' + requisition.getId())
        .statusCode(200);

    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, null);
    verify(processedRequestsRedisRepository, times(1)).addOrUpdate(key, requisition.getId());

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWithUsedIdempotencyKey() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);

    doReturn(ValidationResult.success())
        .when(requisitionService).validateCanApproveRequisition(any(Requisition.class),
        anyUuid());
    doNothing().when(requisition).approve(anyUuid(),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();

    when(processedRequestsRedisRepository.exists(any())).thenReturn(true);

    UUID requisitionId = requisition.getId();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, key)
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(409)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_ALREADY_USED)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWithIdempotencyKeyInWrongFormat() {
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);

    doReturn(ValidationResult.success())
        .when(requisitionService).validateCanApproveRequisition(any(Requisition.class),
        anyUuid());
    doNothing().when(requisition).approve(anyUuid(),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .header(IDEMPOTENCY_KEY_HEADER, wrongFormatKey)
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(MessageKeys.IDEMPOTENCY_KEY_WRONG_FORMAT, wrongFormatKey)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenPeriodEndDateIsInFuture() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);

    doReturn(ValidationResult.success())
        .when(requisitionService).validateCanApproveRequisition(any(Requisition.class),
        anyUuid());
    doNothing().when(requisition).approve(anyUuid(),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();
    ProcessingPeriodDto period = mockPeriod(dateHelper.getCurrentDateWithSystemZone().plusDays(2));

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(ERROR_PERIOD_END_DATE_WRONG, period.getEndDate())),
            MESSAGE, containsString(ISO_DATE.format(period.getEndDate())));

    // then
    verify(requisition, never())
        .submit(anyMapOf(VersionIdentityDto.class, OrderableDto.class), anyUuid(),
        anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenUserHasNoRightForApprove() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();

    String missingPermission = REQUISITION_APPROVE;
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, missingPermission))
        .when(requisitionService).validateCanApproveRequisition(any(Requisition.class), anyUuid());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisition, never()).approve(anyUuid(),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenUserHasNoRightToApproveForSupervisoryNode() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();

    doReturn(ValidationResult.noPermission(ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION,
        requisitionId))
        .when(requisitionService)
        .validateCanApproveRequisition(any(Requisition.class), anyUuid());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION,
            requisitionId, "")));

    // then
    verify(requisition, never()).approve(anyUuid(),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/submitted
  @Test
  public void shouldGetSubmittedRequisitions() {
    // given
    Requisition[] requisitions = {generateRequisition(), generateRequisition()};

    List<OrderableDto> orderables = Arrays
        .stream(requisitions)
        .map(Requisition::getRequisitionLineItems)
        .flatMap(Collection::stream)
        .map(line -> new OrderableDtoDataBuilder()
            .withId(line.getOrderable().getId())
            .withVersionId(line.getOrderable().getVersionId())
            .withProgramOrderable(line.getRequisition().getProgramId(), true)
            .buildAsDto())
        .collect(Collectors.toList());

    given(requisitionService.searchRequisitions(
        any(QueryRequisitionSearchParams.class), any(Pageable.class)))
        .willReturn(Pagination.getPage(Arrays.asList(requisitions), FIRST_PAGE));

    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(orderables);

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(SUBMITTED_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(2, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/periodsForInitiate

  @Test
  public void shouldNotReturnPeriodsForInitiateIfUserHasNoRightsToInitiateRequisition() {
    // given
    String[] errorKeys = {
        PermissionService.REQUISITION_CREATE, PermissionService.REQUISITION_AUTHORIZE};
    doReturn(ValidationResult
        .noPermission(ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION, (Object[]) errorKeys))
        .when(permissionService).canInitOrAuthorizeRequisition(anyUuid(), anyUuid());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam("programId", UUID.randomUUID())
        .queryParam("facilityId", UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(PERIODS_FOR_INITIATE_URL)
        .then()
        .statusCode(403);

    // then
    verify(periodService, never()).getPeriods(anyUuid(), anyUuid(), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/requisitionsForApproval

  @Test
  public void shouldGetRequisitionsForApprovalForSpecificUser() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    List<Requisition> requisitions = Collections.singletonList(requisition);

    given(requisitionService.getRequisitionsForApproval(
        eq(user), eq(null), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, FIRST_PAGE));

    // when
    PageDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(REQ_FOR_APPROVAL_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, result.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetRequisitionsForApprovalForSpecificUserAndProgram() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    List<Requisition> requisitions = Collections.singletonList(requisition);
    UUID program = UUID.randomUUID();

    given(requisitionService.getRequisitionsForApproval(
        eq(user), eq(program), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, FIRST_PAGE));

    // when
    PageDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, program)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(REQ_FOR_APPROVAL_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, result.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/requisitionsForConvert

  @Test
  public void shouldGetApprovedRequisitionsByProgramIdAndFacilityIdWithSortByAndPaging() {
    // given
    RightDto right = mockViewOrdersRight();

    FacilityDto facility = mockFacility();
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);

    RequisitionWithSupplyingDepotsDto requisition =
        new RequisitionWithSupplyingDepotsDto(generateBasicRequisition(), singletonList(facility));

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitions(
        eq(facilityId), eq(programId), any(Pageable.class)))
        .willReturn(Pagination.getPage(singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(FACILITY_ID, facilityId)
        .queryParam(PROGRAM_ID, programId)
        .queryParam(SORT, FACILITY_CODE_ASC)
        .queryParam(PAGE, page)
        .queryParam(SIZE, size)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetApprovedRequisitionsWithSortBySeveralFilters() {
    // given
    RightDto right = mockViewOrdersRight();

    FacilityDto facility = mockFacility();
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);

    RequisitionWithSupplyingDepotsDto requisition =
        new RequisitionWithSupplyingDepotsDto(generateBasicRequisition(), singletonList(facility));

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    int size = 10;
    int page = 0;

    ArgumentCaptor<Pageable> sortByCaptor = ArgumentCaptor.forClass(Pageable.class);

    given(requisitionService.searchApprovedRequisitions(
        eq(facilityId), eq(programId), sortByCaptor.capture()))
        .willReturn(Pagination.getPage(singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(FACILITY_ID, facilityId)
        .queryParam(PROGRAM_ID, programId)
        .queryParam(SORT, "emergency,desc")
        .queryParam(SORT, FACILITY_CODE_ASC)
        .queryParam(PAGE, page)
        .queryParam(SIZE, size)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    Pageable pageable = sortByCaptor.getValue();
    List<Sort.Order> orders = Lists.newArrayList(pageable.getSort());

    assertEquals(2, orders.size());

    assertEquals(Requisition.EMERGENCY_FIELD, orders.get(0).getProperty());
    assertEquals(Sort.Direction.DESC, orders.get(0).getDirection());

    assertEquals("facilityCode", orders.get(1).getProperty());
    assertEquals(Sort.Direction.ASC, orders.get(1).getDirection());
  }

  @Test
  public void shouldGetApprovedRequisitionsWithoutFilters() {
    // given
    RightDto right = mockViewOrdersRight();

    FacilityDto facility = mockFacility();
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);

    RequisitionWithSupplyingDepotsDto requisition =
        new RequisitionWithSupplyingDepotsDto(generateBasicRequisition(), singletonList(facility));

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitions(
        eq(null), eq(null), any(Pageable.class)))
        .willReturn(Pagination.getPage(Collections.singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(SORT, FACILITY_CODE_ASC)
        .queryParam(PAGE, page)
        .queryParam(SIZE, size)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldPassTotalNumberOfRequisitionsForApprovalInResponse() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    List<Requisition> requisitions = Collections.singletonList(requisition);
    long totalElements = 14L;
    Pageable pageable = new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, 1);

    given(requisitionService.getRequisitionsForApproval(
        eq(user), eq(null), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, pageable, totalElements));

    // when
    PageDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PAGE, 0)
        .queryParam(SIZE, 1)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(REQ_FOR_APPROVAL_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, result.getContent().size());
    assertEquals(totalElements, result.getTotalElements());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetApprovedRequisitionsIfUserHasNoFulfillmentRightsForFacility() {
    // given
    given(requisitionService.searchApprovedRequisitions(
        any(), any(), any()))
        .willReturn(Pagination.getPage(Collections.emptyList(), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(0, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetApprovedRequisitionsWithUserFulfillmentRights() {
    // given
    FacilityDto facility = new FacilityDtoDataBuilder().buildAsDto();

    RequisitionWithSupplyingDepotsDto requisition =
        new RequisitionWithSupplyingDepotsDto(generateBasicRequisition(), singletonList(facility));

    given(requisitionService.searchApprovedRequisitions(
        any(), any(), any()))
        .willReturn(Pagination.getPage(singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageDto.class);

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/convertToOrder

  @Test
  public void shouldConvertRequisitionToOrder() {
    // given
    List<ReleasableRequisitionDto> requisitions = singletonList(generateReleasableRequisitionDto());

    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());
    doReturn(new ArrayList<>())
        .when(requisitionService).convertToOrder(any(), any());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(requisitions)
        .when()
        .post(CONVERT_TO_ORDER_URL)
        .then()
        .statusCode(201);

    // then
    verify(requisitionService, atLeastOnce()).convertToOrder(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotConvertRequisitionToOrderWhenConvertToOrderDtoIsInvalid() {
    // given
    List<ReleasableRequisitionDto> requisitions = singletonList(generateReleasableRequisitionDto());

    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());

    String errorKey = MessageKeys.ERROR_CONVERTING_REQUISITION_TO_ORDER;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(requisitionService).convertToOrder(any(), any());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(requisitions)
        .when()
        .post(CONVERT_TO_ORDER_URL)
        .then()
        .statusCode(400);

    // then
    verify(requisitionService, atLeastOnce()).convertToOrder(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private RightDto mockViewOrdersRight() {
    RightDto right = new RightDto();
    right.setId(UUID.randomUUID());
    given(authenticationHelper.getRight(PermissionService.ORDERS_EDIT)).willReturn(right);
    return right;
  }

  private void mockReasons() {
    ReasonDto reasonDto = new ReasonDtoDataBuilder()
        .withReasonCategory(ReasonCategory.ADJUSTMENT)
        .withReasonType(ReasonType.BALANCE_ADJUSTMENT)
        .withDescription("simple description")
        .withFreeTextAllowed(false)
        .withHidden(false)
        .buildAsDto();

    ValidReasonDto validReasonDto = mock(ValidReasonDto.class);
    when(validReasonDto.getReasonWithHidden()).thenReturn(reasonDto);

    when(validReasonStockmanagementService.search(anyUuid(), anyUuid()))
        .thenReturn(singletonList(validReasonDto));
    stockAdjustmentReasons = singletonList(StockAdjustmentReason.newInstance(reasonDto));
  }

  private void mockValidationSuccess() {
    given(statusMessageRepository.save(any(StatusMessage.class))).willReturn(null);

    doNothing().when(requisitionStatusProcessor)
        .statusChange(any(Requisition.class), any(Locale.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(UUID.class), any(UUID.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(FacilityDto.class), any(UUID.class));
  }

  private void mockExternalServiceCalls() {
    given(orderableReferenceDataService.findByIdentities(anySetOf(VersionEntityReference.class)))
        .willReturn(Collections.emptyList());
    given(supervisoryNodeReferenceDataService.findOne(anyUuid()))
        .willReturn(new SupervisoryNodeDto());
  }

  private ProgramDto mockProgram() {
    ProgramDto programDto = DtoGenerator.of(ProgramDto.class);

    given(programReferenceDataService.findOne(anyUuid())).willReturn(programDto);

    return programDto;
  }

  private FacilityDto mockFacility() {
    FacilityDto facilityDto = DtoGenerator.of(FacilityDto.class);
    facilityDto.getType().setId(facilityTypeId);

    when(facilityReferenceDataService.findOne(anyUuid())).thenReturn(facilityDto);

    return facilityDto;
  }

  private ProcessingPeriodDto mockPeriod() {
    return mockPeriod(dateHelper.getCurrentDateWithSystemZone());
  }

  private ProcessingPeriodDto mockPeriod(LocalDate endDate) {
    ProcessingPeriodDto period = DtoGenerator.of(ProcessingPeriodDto.class);
    period.setEndDate(endDate);

    when(periodService.findPeriod(anyUuid(), anyUuid(), anyUuid(), anyBoolean()))
        .thenReturn(period);
    when(periodService.getPeriod(anyUuid()))
        .thenReturn(period);

    return period;
  }

  private void mockFacilityDoesNotSupportProgram(FacilityDto facility, UUID programId) {
    String errorKey = MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(facility, programId);
  }

  private ValidationMessageException mockValidationException(String key, Object... args) {
    ValidationMessageException exception = mock(ValidationMessageException.class);
    Message errorMessage = new Message(key, (Object[]) args);
    given(exception.asMessage()).willReturn(errorMessage);

    return exception;
  }

  private DataRetrievalException mockDataException(String key, Object... args) {
    DataRetrievalException exception = mock(DataRetrievalException.class);
    given(exception.asMessage()).willReturn(new Message(key, (Object[]) args));
    given(exception.getStatus()).willReturn(HttpStatus.NOT_FOUND);

    return exception;
  }

  private ReleasableRequisitionDto generateReleasableRequisitionDto() {
    ReleasableRequisitionDto releasableRequisitionDto = new ReleasableRequisitionDtoDataBuilder()
        .buildAsDto();

    return releasableRequisitionDto;
  }

  private List<Requisition> generateRequisitions(RequisitionStatus... statuses) {
    List<Requisition> requisitions = new ArrayList<>();

    for (RequisitionStatus status : statuses) {
      Requisition requisition = generateRequisition(status);
      requisitions.add(requisition);
    }

    return requisitions;
  }

  private BasicRequisitionDto generateBasicRequisition() {
    Requisition requisition = generateRequisition();
    BasicRequisitionDto basicRequisitionDto = new BasicRequisitionDto();
    requisition.export(basicRequisitionDto);
    return basicRequisitionDto;
  }

  private String getMessage(String messageKey, Object... messageParams) {
    return messageService.localize(new Message(messageKey, messageParams)).asMessage();
  }

}
