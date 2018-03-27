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
import static java.util.Collections.emptyList;
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
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DUPLICATE_STATUS_CHANGE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_END_DATE_WRONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SERVICE_REQUIRED;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_DELETE;

import com.google.common.collect.Lists;
import guru.nidi.ramltester.junit.RamlMatchers;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.PersistenceException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.RequisitionValidationService;
import org.openlmis.requisition.domain.requisition.StatusMessage;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.ReasonType;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.ValidReasonDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.DataRetrievalException;
import org.openlmis.requisition.service.PageDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.service.stockmanagement.ValidReasonStockmanagementService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.RightName;
import org.openlmis.requisition.utils.StockEventBuilder;
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

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

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
  private static final String SUGGESTED_PERIOD = "suggestedPeriod";
  private static final String EMERGENCY = "emergency";
  private static final String MESSAGE = "message";
  private static final String REQUISITION_STATUS = "requisitionStatus";
  private static final String SUPERVISORY_NODE = "supervisoryNode";
  private static final String PROCESSING_PERIOD = "processingPeriod";
  private static final String INITIATED_DATE_FROM = "initiatedDateFrom";
  private static final String INITIATED_DATE_TO = "initiatedDateTo";
  private static final String SORT = "sort";
  private static final String FILTER_VALUE = "filterValue";
  private static final String FILTER_BY = "filterBy";
  private static final String PAGE = "page";
  private static final String FACILITY_CODE_ASC = "facilityCode,asc";

  @MockBean
  private StatusMessageRepository statusMessageRepository;

  @MockBean
  private RequisitionDtoBuilder requisitionDtoBuilder;

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
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @MockBean
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @MockBean
  private OrderableReferenceDataService orderableReferenceDataService;

  @MockBean(name = "facilityReferenceDataService")
  private FacilityReferenceDataService facilityReferenceDataService;

  @MockBean
  private ValidReasonStockmanagementService validReasonStockmanagementService;

  @MockBean
  private StockEventStockManagementService stockEventStockManagementService;

  @MockBean
  private StockEventBuilder stockEventBuilder;

  @MockBean
  private ReasonsValidator reasonsValidator;

  @MockBean
  protected RequisitionVersionValidator requisitionVersionValidator;

  @Autowired
  private MessageService messageService;

  @Autowired
  private DateHelper dateHelper;

  private List<StockAdjustmentReason> stockAdjustmentReasons;
  private UUID facilityTypeId = UUID.randomUUID();

  private Pageable pageRequest = new PageRequest(
      Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION);

  @Before
  public void setUp() {
    mockUserAuthenticated();

    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();
    mockStockEventServiceResponses();

    mockReasons();
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
        .extract().as(RequisitionDto.class);

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
        .validateRequisitionTimestamps(any(Requisition.class), any(Requisition.class)))
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
    Set<RequisitionStatus> statuses = EnumSet.of(RequisitionStatus.INITIATED);

    Requisition requisition = generateRequisition();

    given(requisitionService.searchRequisitions(
        eq(facilityId),
        eq(programId),
        any(LocalDate.class),
        any(LocalDate.class),
        eq(periodId),
        eq(supervisoryNodeId),
        eq(statuses),
        eq(false),
        any(Pageable.class))
    ).willReturn(Pagination.getPage(singletonList(requisition), pageRequest));

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

    Set<RequisitionStatus> statusSet = EnumSet.of(submittedStatus, authorizedStatus);
    List<Requisition> requisitions =
        generateRequisitions(submittedStatus, authorizedStatus);

    given(requisitionService.searchRequisitions(
        eq(null), eq(null), eq(null), eq(null), eq(null),
        eq(null), eq(statusSet), eq(null), any(Pageable.class))
    ).willReturn(Pagination.getPage(requisitions, pageRequest));

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
  public void shouldNotSubmitWhenPeriodEndDateIsInFuture() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.INITIATED);

    doNothing().when(requisition).submit(any(), anyUuid(), anyBoolean());
    doReturn(ValidationResult.success())
        .when(permissionService).canSubmitRequisition(requisition);

    mockExternalServiceCalls();
    mockValidationSuccess();

    ProcessingPeriodDto processingPeriodDto = mock(ProcessingPeriodDto.class);
    when(processingPeriodDto.getEndDate())
        .thenReturn(dateHelper.getCurrentDateWithSystemZone().plusDays(2));
    when(periodReferenceDataService.findOne(requisition.getProcessingPeriodId()))
        .thenReturn(processingPeriodDto);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(ERROR_PERIOD_END_DATE_WRONG,
                processingPeriodDto.getEndDate())));

    // then
    verify(requisition, never()).submit(anyCollectionOf(OrderableDto.class), anyUuid(),
        anyBoolean());
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
    verify(requisition, never()).submit(anyCollectionOf(OrderableDto.class), anyUuid(),
        anyBoolean());
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
    given(requisitionService.skip(requisition)).willReturn(requisition);

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
    verify(requisitionService, atLeastOnce()).skip(requisition);
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
    verify(requisitionService, never()).skip(requisition);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfItIsNotInitiated() {
    testSkipRequisitionValidationFailure(MessageKeys.ERROR_SKIP_FAILED_WRONG_STATUS);
  }

  @Test
  public void shouldNotSkipRequisitionIfItIsEmergency() {
    testSkipRequisitionValidationFailure(MessageKeys.ERROR_SKIP_FAILED_EMERGENCY);
  }

  @Test
  public void shouldReturnNotFoundWhenSkippingNonExistentRequisition() {
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

  // PUT /api/requisitions/{id}/reject

  @Test
  public void shouldRejectRequisition() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    given(requisitionService.reject(requisition)).willReturn(requisition);
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
    verify(requisitionService, atLeastOnce()).reject(requisition);
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
    verify(requisitionService, never()).reject(requisition);
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
    doThrow(exception).when(requisitionService).reject(requisition);

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
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();
    SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
    given(supervisoryNode.getId()).willReturn(UUID.randomUUID());

    given(supervisoryNodeReferenceDataService.findSupervisoryNode(programId, facilityId))
        .willReturn(supervisoryNode);
    given(orderableReferenceDataService.findByIds(anySetOf(UUID.class)))
        .willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyCollectionOf(OrderableDto.class), anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(200);

    // then
    verify(requisition, atLeastOnce()).authorize(anyCollectionOf(OrderableDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeRequisitionWhenPeriodEndDateIsInFuture() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.SUBMITTED);
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();
    SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
    given(supervisoryNode.getId()).willReturn(UUID.randomUUID());

    given(supervisoryNodeReferenceDataService.findSupervisoryNode(programId, facilityId))
        .willReturn(supervisoryNode);
    given(orderableReferenceDataService.findByIds(anySetOf(UUID.class)))
        .willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyCollectionOf(OrderableDto.class), anyUuid());
    doReturn(ValidationResult.success()).when(permissionService)
        .canAuthorizeRequisition(requisition);
    mockValidationSuccess();

    ProcessingPeriodDto processingPeriodDto = mock(ProcessingPeriodDto.class);
    when(processingPeriodDto.getEndDate())
        .thenReturn(dateHelper.getCurrentDateWithSystemZone().plusDays(2));
    when(periodReferenceDataService.findOne(requisition.getProcessingPeriodId()))
        .thenReturn(processingPeriodDto);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(ERROR_PERIOD_END_DATE_WRONG,
                processingPeriodDto.getEndDate())));

    // then
    verify(requisition, never()).submit(anyCollectionOf(OrderableDto.class), anyUuid(),
        anyBoolean());
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
    verify(requisition, never()).authorize(anyCollectionOf(OrderableDto.class), any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/{id}/initiate
  @Test
  public void shouldInitiateRequisition() {
    // given
    ProgramDto program = mockProgram();
    FacilityDto facility = mockFacility();
    Requisition requisition =
        generateRequisition(RequisitionStatus.INITIATED, program.getId(), facility.getId());
    UUID periodId = requisition.getProcessingPeriodId();

    doReturn(ValidationResult.success())
        .when(permissionService)
        .canInitRequisition(program.getId(), facility.getId());
    doReturn(requisition)
        .when(requisitionService)
        .initiate(eq(program), eq(facility), eq(periodId), eq(false),
            anyListOf(StockAdjustmentReason.class));
    mockValidationSuccess();

    // when
    RequisitionDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam(SUGGESTED_PERIOD, periodId)
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
        .initiate(program, facility, periodId, false, stockAdjustmentReasons);
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
    verify(requisitionService, never())
        .initiate(any(ProgramDto.class), any(FacilityDto.class), anyUuid(), anyBoolean(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.requestChecks());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenPeriodDoesNotExist() {
    // given
    ProgramDto program = mockProgram();
    FacilityDto facility = mockFacility();
    UUID periodId = UUID.randomUUID();

    ValidationMessageException err =
        mockValidationException(MessageKeys.ERROR_INCORRECT_SUGGESTED_PERIOD);
    given(requisitionService
        .initiate(eq(program), eq(facility), eq(periodId), eq(false),
            anyListOf(StockAdjustmentReason.class)))
        .willThrow(err);

    doReturn(ValidationResult.success())
        .when(permissionService).canInitRequisition(program.getId(), facility.getId());
    mockValidationSuccess();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(PROGRAM, program)
        .queryParam(FACILITY, facility)
        .queryParam(SUGGESTED_PERIOD, periodId)
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
            anyUuid(), anyBoolean(), anyList());
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
            anyUuid(), anyBoolean(), anyList());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenPeriodEndDateIsInFuture() {
    // given
    Requisition requisition = spyRequisitionAndStubRepository(RequisitionStatus.AUTHORIZED);

    doReturn(ValidationResult.success())
        .when(requisitionService).validateCanApproveRequisition(any(Requisition.class),
        anyUuid());
    doNothing().when(requisition).approve(anyUuid(), anyCollectionOf(OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();

    ProcessingPeriodDto processingPeriodDto = mock(ProcessingPeriodDto.class);
    when(processingPeriodDto.getEndDate())
        .thenReturn(dateHelper.getCurrentDateWithSystemZone().plusDays(2));
    when(periodReferenceDataService.findOne(requisition.getProcessingPeriodId()))
        .thenReturn(processingPeriodDto);

    UUID requisitionId = requisition.getId();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE,
            equalTo(getMessage(ERROR_PERIOD_END_DATE_WRONG, processingPeriodDto.getEndDate())),
            MESSAGE, containsString(ISO_DATE.format(processingPeriodDto.getEndDate())));

    // then
    verify(requisition, never()).submit(anyCollectionOf(OrderableDto.class), anyUuid(),
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
    verify(requisition, never()).approve(anyUuid(), anyCollectionOf(OrderableDto.class),
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
    verify(requisition, never()).approve(anyUuid(), anyCollectionOf(OrderableDto.class),
        anyCollectionOf(SupplyLineDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/submitted
  @Test
  public void shouldGetSubmittedRequisitions() {
    // given
    Requisition[] requisitions = {generateRequisition(), generateRequisition()};
    given(requisitionService.searchRequisitions(
        anySetOf(RequisitionStatus.class), any(Pageable.class)))
        .willReturn(Pagination.getPage(Arrays.asList(requisitions), pageRequest));

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

    UUID userId = authenticationHelper.getCurrentUser().getId();
    given(requisitionService.getRequisitionsForApproval(
        eq(userId), eq(null), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, pageRequest));

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

    UUID userId = authenticationHelper.getCurrentUser().getId();
    given(requisitionService.getRequisitionsForApproval(
        eq(userId), eq(program), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, pageRequest));

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
  public void shouldGetApprovedRequisitionsWithSortByFilterByAndPaging() {
    // given
    RightDto right = mockViewOrdersRight();

    FacilityDto facility = mockFacility();
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);
    List<UUID> managedFacilitiesIds = singletonList(facility.getId());

    RequisitionWithSupplyingDepotsDto requisition = new RequisitionWithSupplyingDepotsDto();
    requisition.setRequisition(generateBasicRequisition());

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    List<String> filterValue = Lists.newArrayList("Hospital");
    String filterBy = "facilityName";
    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        eq(filterValue), eq(filterBy), any(Pageable.class), eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(FILTER_VALUE, filterValue)
        .queryParam(FILTER_BY, filterBy)
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
  public void shouldGetApprovedRequisitionsWithSortByMultipleFilterValues() {
    // given
    RightDto right = mockViewOrdersRight();

    FacilityDto facility = mockFacility();
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);
    List<UUID> managedFacilitiesIds = Collections.singletonList(facility.getId());

    RequisitionWithSupplyingDepotsDto requisition = new RequisitionWithSupplyingDepotsDto();
    requisition.setRequisition(generateBasicRequisition());

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    List<String> filterValue = Lists.newArrayList("Essential Meds", "Family Planning");
    String filterBy = "programName";
    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        eq(filterValue), eq(filterBy), any(Pageable.class), eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(Collections.singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(FILTER_VALUE, filterValue)
        .queryParam(FILTER_BY, filterBy)
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
    List<UUID> managedFacilitiesIds = singletonList(facility.getId());

    RequisitionWithSupplyingDepotsDto requisition = new RequisitionWithSupplyingDepotsDto();
    requisition.setRequisition(generateBasicRequisition());

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    List<String> filterValue = Lists.newArrayList("Hospital");
    String filterBy = "facilityName";
    int size = 10;
    int page = 0;

    ArgumentCaptor<Pageable> sortByCaptor = ArgumentCaptor.forClass(Pageable.class);

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        eq(filterValue), eq(filterBy), sortByCaptor.capture(), eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(singletonList(requisition), null));

    // when
    PageDto response = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam(FILTER_VALUE, filterValue)
        .queryParam(FILTER_BY, filterBy)
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
  public void shouldPassTotalNumberOfRequisitionsForApprovalInResponse() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    List<Requisition> requisitions = Collections.singletonList(requisition);
    long totalElements = 14L;
    Pageable pageable = new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, 1);

    UUID userId = authenticationHelper.getCurrentUser().getId();
    given(requisitionService.getRequisitionsForApproval(
        eq(userId), eq(null), any(Pageable.class)))
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
    mockConvertToOrderRightAndFulfillmentFacilities();
    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        any(), any(), any(), eq(Collections.emptyList())))
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
    FacilityDto facility = new FacilityDto();
    facility.setId(UUID.randomUUID());
    List<UUID> managedFacilitiesIds = mockConvertToOrderRightAndFulfillmentFacilities(facility);

    RequisitionWithSupplyingDepotsDto requisition = new RequisitionWithSupplyingDepotsDto();
    requisition.setRequisition(generateBasicRequisition());

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        any(), any(), any(), eq(managedFacilitiesIds)))
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
    List<ConvertToOrderDto> requisitions = singletonList(generateConvertToOrderDto());

    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());
    doNothing().when(requisitionService).convertToOrder(any(), any());

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
    List<ConvertToOrderDto> requisitions = singletonList(generateConvertToOrderDto());

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
    given(authenticationHelper.getRight(RightName.ORDERS_EDIT)).willReturn(right);
    return right;
  }

  private void mockReasons() {
    ReasonDto reasonDto = new ReasonDto();
    reasonDto.setId(UUID.randomUUID());
    reasonDto.setReasonCategory(ReasonCategory.ADJUSTMENT);
    reasonDto.setReasonType(ReasonType.BALANCE_ADJUSTMENT);
    reasonDto.setDescription("simple description");
    reasonDto.setIsFreeTextAllowed(false);
    reasonDto.setName("simple name");
    reasonDto.setHidden(false);

    ValidReasonDto validReasonDto = mock(ValidReasonDto.class);
    when(validReasonDto.getReasonWithHidden()).thenReturn(reasonDto);

    when(validReasonStockmanagementService.search(anyUuid(), anyUuid()))
        .thenReturn(singletonList(validReasonDto));
    stockAdjustmentReasons = singletonList(StockAdjustmentReason.newInstance(reasonDto));
  }

  private void testSkipRequisitionValidationFailure(String messageKey, Object... messageParams) {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    doReturn(ValidationResult.success())
        .when(permissionService).canUpdateRequisition(requisition);

    ValidationMessageException exception = mockValidationException(messageKey, messageParams);
    doThrow(exception).when(requisitionService).skip(requisition);

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(messageKey, messageParams)));

    // then
    verify(requisitionService, atLeastOnce()).skip(requisition);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private void mockValidationSuccess() {
    given(statusMessageRepository.save(any(StatusMessage.class))).willReturn(null);

    doNothing().when(requisitionStatusProcessor).statusChange(any(Requisition.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(UUID.class), any(UUID.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(FacilityDto.class), any(UUID.class));
  }

  private void mockExternalServiceCalls() {
    given(orderableReferenceDataService.findByIds(anySetOf(UUID.class)))
        .willReturn(Collections.emptyList());
    given(supervisoryNodeReferenceDataService.findOne(anyUuid()))
        .willReturn(new SupervisoryNodeDto());
  }

  private ProgramDto mockProgram() {
    ProgramDto programDto = new ProgramDto();
    programDto.setId(UUID.randomUUID());
    given(programReferenceDataService.findOne(anyUuid()))
        .willReturn(programDto);
    return programDto;
  }

  private FacilityDto mockFacility() {
    FacilityTypeDto facilityTypeDto = new FacilityTypeDto();
    facilityTypeDto.setId(facilityTypeId);

    FacilityDto facilityDto = new FacilityDto();
    facilityDto.setId(UUID.randomUUID());
    facilityDto.setType(facilityTypeDto);

    when(facilityReferenceDataService.findOne(anyUuid()))
        .thenReturn(facilityDto);

    return facilityDto;
  }

  private void mockFacilityDoesNotSupportProgram(FacilityDto facility, UUID programId) {
    String errorKey = MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(facility, programId);
  }

  private List<UUID> mockConvertToOrderRightAndFulfillmentFacilities(FacilityDto... facilities) {
    RightDto right = mockViewOrdersRight();

    Set<FacilityDto> facilitySet = new HashSet<>();
    Collections.addAll(facilitySet, facilities);

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        anyUuid(), eq(right.getId()))).willReturn(facilitySet);

    return facilitySet
        .stream()
        .map(FacilityDto::getId)
        .collect(Collectors.toList());
  }

  private void mockRequisitionDtoBuilderResponses() {
    given(requisitionDtoBuilder.build(any(Requisition.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder
        .build(any(Requisition.class), any(FacilityDto.class), any(ProgramDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder
        .build(any(Requisition.class), anyMap(), any(FacilityDto.class), any(ProgramDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.build(anyListOf(Requisition.class)))
        .willAnswer(new BuildListOfRequisitionDtosAnswer());
  }

  private void mockRepositorySaveAnswer() {
    given(requisitionRepository.save(any(Requisition.class))).willAnswer(new SaveAnswer<>());
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

  private void mockStockEventServiceResponses() {
    when(stockEventBuilder.fromRequisition(any())).thenReturn(new StockEventDto());
    doNothing().when(stockEventStockManagementService).submit(any(StockEventDto.class));
  }

  private ConvertToOrderDto generateConvertToOrderDto() {
    ConvertToOrderDto convertDto = new ConvertToOrderDto();
    convertDto.setSupplyingDepotId(UUID.randomUUID());
    convertDto.setRequisitionId(UUID.randomUUID());

    return convertDto;
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

  protected static class BuildRequisitionDtoAnswer implements Answer<RequisitionDto> {

    @Override
    public RequisitionDto answer(InvocationOnMock invocation) throws Throwable {
      Requisition requisition = (Requisition) invocation.getArguments()[0];

      if (null == requisition) {
        return null;
      }

      return export(requisition);
    }

    public static RequisitionDto export(Requisition requisition) {
      RequisitionDto dto = new RequisitionDto();
      requisition.export(dto);

      dto.setTemplate(BasicRequisitionTemplateDto.newInstance(requisition.getTemplate()));
      dto.setRequisitionLineItems(Collections.emptyList());

      FacilityDto facility = null;
      if (requisition.getFacilityId() != null) {
        facility = new FacilityDto();
        facility.setId(requisition.getFacilityId());
      }

      ProgramDto program = null;
      if (requisition.getProgramId() != null) {
        program = new ProgramDto();
        program.setId(requisition.getProgramId());
      }

      ProcessingPeriodDto period = null;
      if (requisition.getSupervisoryNodeId() != null) {
        period = new ProcessingPeriodDto();
        period.setId(requisition.getProcessingPeriodId());
      }

      dto.setProcessingPeriod(period);
      dto.setFacility(facility);
      dto.setProgram(program);

      return dto;
    }
  }

  protected static class BuildListOfRequisitionDtosAnswer implements Answer<List<RequisitionDto>> {

    @Override
    public List<RequisitionDto> answer(InvocationOnMock invocation) throws Throwable {
      Collection<Requisition> collection = (Collection) invocation.getArguments()[0];

      if (null == collection) {
        return emptyList();
      }

      return collection
          .stream()
          .map(BuildRequisitionDtoAnswer::export)
          .collect(Collectors.toList());
    }
  }
}
