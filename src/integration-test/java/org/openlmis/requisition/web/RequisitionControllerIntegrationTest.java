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

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_DELETE;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.FacilitySupportsProgramHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.PageImplRepresentation;
import org.openlmis.utils.Pagination;
import org.openlmis.utils.RightName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.validation.BindingResult;
import org.springframework.validation.Errors;
import java.time.ZonedDateTime;
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

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

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
  static final String PROCESSING_PERIOD = "processingPeriod";
  static final String INITIATED_DATE_FROM = "initiatedDateFrom";
  static final String INITIATED_DATE_TO = "initiatedDateTo";

  @MockBean
  private StatusMessageRepository statusMessageRepository;

  @MockBean
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @MockBean
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @MockBean
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @MockBean
  private RequisitionValidator requisitionValidator;

  @MockBean
  private PeriodService periodService;

  @MockBean
  private RequisitionService requisitionService;

  @MockBean
  private ConfigurationSettingService configurationSettingService;

  @MockBean
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @MockBean
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @MockBean
  private OrderableReferenceDataService orderableReferenceDataService;

  @MockBean(name = "programReferenceDataService")
  private ProgramReferenceDataService programReferenceDataService;

  @MockBean(name = "facilityReferenceDataService")
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private MessageService messageService;

  @Before
  public void setUp() {
    mockUserAuthenticated();

    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();
  }

  // GET /api/requisitions/{id}

  @Test
  public void shouldGetChosenRequisition() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    doNothing().when(permissionService).canViewRequisition(requisition.getId());

    // when
    RequisitionDto result = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canViewRequisition(anyUuid());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    doNothing().when(permissionService).canViewRequisition(anyUuid());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // DELETE /api/requisitions/{id}

  @Test
  public void shouldDeleteRequisition() {
    // given
    Requisition requisition = generateRequisition();
    doNothing().when(permissionService).canDeleteRequisition(requisition.getId());
    doNothing().when(requisitionService).delete(requisition.getId());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(204);

    // then
    verify(requisitionService, atLeastOnce()).delete(requisition.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionWhenUserHasNoRightForDelete() {
    // given
    Requisition requisition = generateRequisition();
    doNothing().when(permissionService).canDeleteRequisition(requisition.getId());

    String missingPermission = REQUISITION_DELETE;
    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canDeleteRequisition(requisition.getId());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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

    String errorKey = MessageKeys.ERROR_REQUISITION_NOT_FOUND;
    ContentNotFoundMessageException exception = mockNotFoundException(errorKey, requisitionId);
    doThrow(exception).when(permissionService).canDeleteRequisition(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    doNothing().when(permissionService).canDeleteRequisition(requisition.getId());

    String errorKey = MessageKeys.ERROR_DELETE_FAILED_WRONG_STATUS;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(requisitionService).delete(requisition.getId());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    ZonedDateTime dateTo = ZonedDateTime.now().plusDays(10);
    ZonedDateTime dateFrom = ZonedDateTime.now().minusDays(10);
    Set<RequisitionStatus> statuses = EnumSet.of(RequisitionStatus.INITIATED);

    Requisition requisition = generateRequisition();
    doNothing().when(permissionService).canViewRequisition(requisition.getId());

    given(requisitionService.searchRequisitions(
        eq(facilityId),
        eq(programId),
        any(ZonedDateTime.class),
        any(ZonedDateTime.class),
        eq(periodId),
        eq(supervisoryNodeId),
        eq(statuses),
        eq(false))
    ).willReturn(Collections.singletonList(requisition));

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
        .extract().as(PageImplRepresentation.class);

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
        generateRequisitionsWithMockedAccess(submittedStatus, authorizedStatus);

    given(requisitionService.searchRequisitions(
        eq(null), eq(null), eq(null), eq(null), eq(null),
        eq(null), eq(statusSet), eq(null))
    ).willReturn(requisitions);

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(REQUISITION_STATUS, submittedStatus)
        .queryParam(REQUISITION_STATUS, authorizedStatus)
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(2, resultPage.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnOnlyRequisitionsForWhichUserHasRights() {
    // given
    Requisition accessibleRequisition = generateRequisition(RequisitionStatus.INITIATED);
    doNothing().when(permissionService).canViewRequisition(accessibleRequisition.getId());

    Requisition inaccessibleRequisition = generateRequisition(RequisitionStatus.INITIATED);
    doThrow(PermissionMessageException.class)
        .when(permissionService).canViewRequisition(inaccessibleRequisition.getId());

    List<Requisition> requisitions = Arrays.asList(accessibleRequisition, inaccessibleRequisition);
    given(requisitionService.searchRequisitions(
        eq(null), eq(null), eq(null), eq(null), eq(null),
        eq(null), eq(null), eq(null))
    ).willReturn(requisitions);

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(1, resultPage.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnEmptyListIfUserHasNoRightsToSeeFoundRequisitions() {
    // given
    Requisition inaccessibleRequisition = generateRequisition(RequisitionStatus.INITIATED);
    doThrow(PermissionMessageException.class)
        .when(permissionService).canViewRequisition(inaccessibleRequisition.getId());

    List<Requisition> requisitions = Collections.singletonList(inaccessibleRequisition);
    given(requisitionService.searchRequisitions(
        eq(null), eq(null), eq(null), eq(null), eq(null),
        eq(null), eq(null), eq(null))
    ).willReturn(requisitions);

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertTrue(resultPage.getContent().isEmpty());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/{id}/submit

  @Test
  public void shouldSubmitValidRequisition() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.INITIATED);
    UUID requisitionId = requisition.getId();

    doNothing().when(requisition).submit(any(), anyUuid());
    doNothing().when(permissionService).canViewRequisition(requisitionId);

    mockExternalServiceCalls();
    mockValidationSuccess();

    // when
    RequisitionDto result = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto.class);

    // then
    assertEquals(requisitionId, result.getId());
    verify(requisition, atLeastOnce()).submit(any(), any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWhenUserHasNoRightForSubmit() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.INITIATED);
    UUID requisitionId = requisition.getId();

    String missingPermission = REQUISITION_CREATE;
    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canSubmitRequisition(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisition, never()).submit(anyCollectionOf(OrderableDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitIfFacilityDoesNotSupportProgram() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.INITIATED);
    UUID requisitionId = requisition.getId();

    doNothing().when(permissionService).canSubmitRequisition(requisitionId);
    doNothing().when(requisitionValidator).validate(eq(requisition), any(BindingResult.class));

    mockFacilityDoesNotSupportProgram(requisition);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM)));

    // then
    verify(requisition, never()).submit(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/skip

  @Test
  public void shouldSkipRequisition() {
    // given
    Requisition requisition = generateRequisition();
    UUID requisitionId = requisition.getId();

    doNothing().when(permissionService).canViewRequisition(requisitionId);
    mockValidationSuccess();

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);
    given(requisitionService.skip(requisition.getId())).willReturn(requisition);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(200);

    // then
    verify(requisitionService, atLeastOnce()).skip(requisitionId);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfUserHasNoRightForCreate() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);

    String missingPermission = REQUISITION_CREATE;

    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canUpdateRequisition(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisitionService, never()).skip(requisition.getId());
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

    String errorKey = MessageKeys.ERROR_REQUISITION_NOT_FOUND;
    ContentNotFoundMessageException exception = mockNotFoundException(errorKey, requisitionId);
    doThrow(exception).when(requisitionService).skip(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(404)
        .body(MESSAGE, equalTo(getMessage(errorKey, requisitionId)));

    // then
    verify(requisitionRepository, never()).save(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/reject

  @Test
  public void shouldRejectRequisition() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    given(requisitionService.reject(requisition.getId())).willReturn(requisition);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(200);

    // then
    verify(requisitionService, atLeastOnce()).reject(requisition.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectRequisitionWhenUserHasNoRightForApprove() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.AUTHORIZED));
    UUID requisitionId = requisition.getId();

    String missingPermission = REQUISITION_APPROVE;

    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canApproveRequisition(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisitionService, never()).reject(requisitionId);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectRequisitionWithWrongStatus() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

    doNothing().when(permissionService).canUpdateRequisition(requisitionId);

    String errorKey = MessageKeys.ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL;

    ValidationMessageException exception = mockValidationException(errorKey, requisitionId);
    doThrow(exception).when(requisitionService).reject(requisition.getId());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    Requisition requisition = spyRequisition(RequisitionStatus.SUBMITTED);
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();
    SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
    given(supervisoryNode.getId()).willReturn(UUID.randomUUID());

    given(configurationSettingService.getBoolValue(any(String.class))).willReturn(false);
    given(supervisoryNodeReferenceDataService.findSupervisoryNode(programId, facilityId))
        .willReturn(supervisoryNode);
    given(orderableReferenceDataService.findAll()).willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyCollectionOf(OrderableDto.class), anyUuid());
    mockValidationSuccess();

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
  public void shouldNotAuthorizeRequisitionWhenUserHasNoRightForAuthorize() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.SUBMITTED);
    UUID requisitionId = requisition.getId();

    String missingPermission = REQUISITION_AUTHORIZE;
    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canAuthorizeRequisition(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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

  @Test
  public void shouldNotAuthorizeWhenAuthorizationIsSkipped() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.SUBMITTED);
    UUID requisitionId = requisition.getId();

    doNothing().when(permissionService).canApproveRequisition(requisitionId);
    given(configurationSettingService.getBoolValue("skipAuthorization")).willReturn(true);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(400);

    // then
    verify(requisition, never()).authorize(anyCollectionOf(OrderableDto.class), any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeIfFacilityDoesNotSupportProgram() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.SUBMITTED));
    UUID requisitionId = requisition.getId();
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();

    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

    doNothing().when(permissionService).canAuthorizeRequisition(requisitionId);
    doNothing().when(requisitionValidator).validate(eq(requisition), any(BindingResult.class));
    given(configurationSettingService.getBoolValue(anyString())).willReturn(false);

    String errorKey = MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(facilityId, programId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    verify(requisition, never()).authorize(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/{id}/initiate

  @Test
  public void shouldInitiateRequisition() {
    // given
    UUID programId = mockProgram().getId();
    UUID facilityId = mockFacility().getId();
    Requisition requisition =
        generateRequisition(RequisitionStatus.INITIATED, programId, facilityId);
    UUID periodId = requisition.getProcessingPeriodId();

    doNothing().when(permissionService).canInitRequisition(programId, facilityId);
    doReturn(requisition).when(requisitionService).initiate(programId, facilityId, periodId, false);
    mockValidationSuccess();

    // when
    RequisitionDto result = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityId)
        .queryParam(SUGGESTED_PERIOD, periodId)
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(201)
        .extract().as(RequisitionDto.class);

    // then
    assertEquals(requisition.getId(), result.getId());
    verify(requisitionService, atLeastOnce()).initiate(programId, facilityId, periodId, false);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenPeriodDoesNotExist() {
    // given
    UUID programId = mockProgram().getId();
    UUID facilityId = mockFacility().getId();
    UUID periodId = UUID.randomUUID();

    ValidationMessageException err =
        mockValidationException(MessageKeys.ERROR_INCORRECT_SUGGESTED_PERIOD);
    given(requisitionService.initiate(programId, facilityId, periodId, false)).willThrow(err);

    doNothing().when(permissionService).canInitRequisition(programId, facilityId);
    mockValidationSuccess();

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityId)
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
    mockFacilityDoesNotSupportProgram(facilityDto, programId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    verify(requisitionService, never()).initiate(anyUuid(), anyUuid(), anyUuid(), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenUserHasNoRight() {
    // given
    UUID programId = mockProgram().getId();
    UUID facilityId = mockFacility().getId();

    String missingPermission = REQUISITION_CREATE;
    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canInitRequisition(programId, facilityId);

    // then
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    verify(requisitionService, never()).initiate(anyUuid(), anyUuid(), anyUuid(), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/approve

  @Test
  public void shouldApproveSubmittedRequisitionWhenSkippedAuthorization() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();

    given(configurationSettingService.getBoolValue("skipAuthorization")).willReturn(true);
    doNothing().when(permissionService).canApproveRequisition(requisitionId);
    doNothing().when(requisition).approve(anyUuid(), anyCollectionOf(OrderableDto.class),
        anyUuid());

    mockExternalServiceCalls();
    mockValidationSuccess();

    // when
    RequisitionDto result = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto.class);

    // then
    assertEquals(requisitionId, result.getId());
    verify(requisition, atLeastOnce()).approve(anyUuid(), anyCollectionOf(OrderableDto.class),
        anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenUserHasNoRightForApprove() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();

    String missingPermission = REQUISITION_APPROVE;
    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canApproveRequisition(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisition, never()).approve(anyUuid(), anyCollectionOf(OrderableDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenUserHasNoRightToApproveForSupervisoryNode() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();

    PermissionMessageException exception = mock(PermissionMessageException.class);
    Message errorMessage = new Message(ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION);
    given(exception.asMessage()).willReturn(errorMessage);
    doThrow(exception).when(requisitionService).canApproveRequisition(
        any(UUID.class),
        any(UUID.class),
        any(UUID.class));

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION,
            requisitionId, "")));

    // then
    verify(requisition, never()).approve(anyUuid(), anyCollectionOf(OrderableDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveWhenFacilityDoesNotSupportProgram() {
    // given
    Requisition requisition = spyRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();

    doNothing().when(permissionService).canAuthorizeRequisition(requisitionId);
    doNothing().when(requisitionValidator).validate(eq(requisition), any(BindingResult.class));
    given(configurationSettingService.getBoolValue(anyString())).willReturn(false);

    mockFacilityDoesNotSupportProgram(requisition);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM)));

    // then
    verify(requisition, never()).approve(anyUuid(), anyCollectionOf(OrderableDto.class), anyUuid());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/submitted

  @Test
  public void shouldGetSubmittedRequisitions() {
    // given
    Requisition[] requisitions = { generateRequisition(), generateRequisition() };
    given(requisitionService.searchRequisitions(
        anySetOf(RequisitionStatus.class)))
        .willReturn(Arrays.asList(requisitions));

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(SUBMITTED_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(2, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnOnlySubmittedRequisitionsForWhichUserHasRight() {
    // given
    Requisition availableRequisition = generateRequisition(RequisitionStatus.SUBMITTED);
    Requisition unavailableRequisition = generateRequisition(RequisitionStatus.SUBMITTED);
    Requisition[] requisitions = { availableRequisition, unavailableRequisition };

    given(requisitionService.searchRequisitions(
        anySetOf(RequisitionStatus.class)))
        .willReturn(Arrays.asList(requisitions));

    PermissionMessageException exception = mock(PermissionMessageException.class);
    doThrow(exception).when(permissionService).canViewRequisition(unavailableRequisition.getId());

    // when
    PageImplRepresentation<RequisitionDto> response = new PageImplRepresentation<>();
    response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(SUBMITTED_URL)
        .then()
        .statusCode(200)
        .extract().as(response.getClass());

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnEmptyListWhenUserHasNoRightsToSeeSubmittedRequisitions() {
    // given
    List<Requisition> requisitions = Arrays.asList(
        generateRequisition(RequisitionStatus.SUBMITTED),
        generateRequisition(RequisitionStatus.SUBMITTED));

    PermissionMessageException exception = mock(PermissionMessageException.class);
    doThrow(exception).when(permissionService).canViewRequisition(anyUuid());

    given(requisitionService.searchRequisitions(
        eq(EnumSet.of(RequisitionStatus.SUBMITTED))))
        .willReturn(requisitions);

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(SUBMITTED_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(0, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/periodsForInitiate

  @Test
  public void shouldNotReturnPeriodsForInitiateIfUserHasNoRightsToInitiateRequisition() {
    // given
    String[] errorKeys = {
        PermissionService.REQUISITION_CREATE, PermissionService.REQUISITION_AUTHORIZE };
    PermissionMessageException exception = mockPermissionException(errorKeys);
    doThrow(exception).when(permissionService).canInitOrAuthorizeRequisition(anyUuid(), anyUuid());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("programId", UUID.randomUUID())
        .queryParam("facilityId", UUID.randomUUID())
        .queryParam("emergency", false)
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
    Set<Requisition> requisitions = Collections.singleton(requisition);

    UUID userId = authenticationHelper.getCurrentUser().getId();
    given(requisitionService.getRequisitionsForApproval(userId, null)).willReturn(requisitions);

    // when
    PageImplRepresentation result = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(REQ_FOR_APPROVAL_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(1, result.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetRequisitionsForApprovalForSpecificUserAndProgram() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    Set<Requisition> requisitions = Collections.singleton(requisition);
    UUID program = UUID.randomUUID();

    UUID userId = authenticationHelper.getCurrentUser().getId();
    given(requisitionService.getRequisitionsForApproval(userId, program)).willReturn(requisitions);

    // when
    PageImplRepresentation result = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .queryParam(PROGRAM, program)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .when()
            .get(REQ_FOR_APPROVAL_URL)
            .then()
            .statusCode(200)
            .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(1, result.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/requisitionsForConvert

  @Test
  public void shouldGetApprovedRequisitionsWithSortByFilterByAndPaging() {
    // given
    RightDto right = new RightDto();
    right.setId(UUID.randomUUID());
    given(authenticationHelper.getRight(RightName.ORDERS_EDIT)).willReturn(right);

    FacilityDto facility = new FacilityDto();
    facility.setId(UUID.randomUUID());
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);
    List<UUID> managedFacilitiesIds = Collections.singletonList(facility.getId());

    RequisitionWithSupplyingDepotsDto requisition = new RequisitionWithSupplyingDepotsDto();

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    String filterValue = "Hospital";
    String filterBy = "facilityName";
    String sortBy = "facilityCode";
    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        eq(filterValue), eq(filterBy), eq(sortBy), eq(false), any(Pageable.class),
        eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(Collections.singletonList(requisition), null));

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", filterValue)
        .queryParam("filterBy", filterBy)
        .queryParam("sortBy", sortBy)
        .queryParam("descending", Boolean.FALSE.toString())
        .queryParam("page", page)
        .queryParam("size", size)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetApprovedRequisitionsIfUserHasNoFulfillmentRightsForFacility() {
    // given
    mockConvertToOrderRightAndFulfillmentFacilities();
    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        any(), any(), any(), any(), any(), eq(Collections.emptyList())))
        .willReturn(Pagination.getPage(Collections.emptyList(), null));

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

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
    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        any(), any(), any(), any(), any(), eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(Collections.singletonList(requisition), null));

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/convertToOrder

  @Test
  public void shouldConvertRequisitionToOrder() {
    // given
    List<ConvertToOrderDto> requisitions = Collections.singletonList(generateConvertToOrderDto());

    doNothing().when(permissionService).canConvertToOrder(eq(requisitions));
    doNothing().when(requisitionService).convertToOrder(any(), any());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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
    List<ConvertToOrderDto> requisitions = Collections.singletonList(generateConvertToOrderDto());

    doNothing().when(permissionService).canConvertToOrder(eq(requisitions));

    String errorKey = MessageKeys.ERROR_CONVERTING_REQUISITION_TO_ORDER;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(requisitionService).convertToOrder(any(), any());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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

  private void testSkipRequisitionValidationFailure(String messageKey, Object... messageParams) {
    // given
    doNothing().when(permissionService).canUpdateRequisition(any(UUID.class));

    ValidationMessageException exception = mockValidationException(messageKey, messageParams);
    doThrow(exception).when(requisitionService).skip(any(UUID.class));

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(messageKey, messageParams)));

    // then
    verify(requisitionService, atLeastOnce()).skip(any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private void mockValidationSuccess() {
    given(statusMessageRepository.save(any(StatusMessage.class))).willReturn(null);

    doNothing().when(requisitionStatusProcessor).statusChange(any(Requisition.class));
    doNothing().when(requisitionValidator).validate(any(Object.class), any(Errors.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(UUID.class), any(UUID.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(FacilityDto.class), any(UUID.class));
  }

  private void mockExternalServiceCalls() {
    given(orderableReferenceDataService.findAll()).willReturn(Collections.emptyList());
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
    FacilityDto facilityDto = new FacilityDto();
    facilityDto.setId(UUID.randomUUID());
    given(facilityReferenceDataService.findOne(anyUuid()))
        .willReturn(facilityDto);
    return facilityDto;
  }

  private void mockFacilityDoesNotSupportProgram(Requisition requisition) {
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();

    String errorKey = MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(facilityId, programId);
  }

  private void mockFacilityDoesNotSupportProgram(FacilityDto facility, UUID programId) {
    String errorKey = MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(facility, programId);
  }

  private List<UUID> mockConvertToOrderRightAndFulfillmentFacilities(FacilityDto... facilities) {
    RightDto right = new RightDto();
    right.setId(UUID.randomUUID());
    given(authenticationHelper.getRight(RightName.ORDERS_EDIT)).willReturn(right);

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
    given(requisitionDtoBuilder.build(anyListOf(Requisition.class)))
        .willAnswer(new BuildListOfRequisitionDtosAnswer());
  }

  private void mockRepositorySaveAnswer() {
    given(requisitionRepository.save(any(Requisition.class))).willAnswer(new SaveAnswer<>());
  }

  private Requisition spyRequisition(RequisitionStatus status) {
    Requisition requisition = spy(generateRequisition(status));
    UUID requisitionId = requisition.getId();

    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);
    return requisition;
  }

  private ValidationMessageException mockValidationException(String key, Object... args) {
    ValidationMessageException exception = mock(ValidationMessageException.class);
    Message errorMessage = new Message(key, (Object[])args);
    given(exception.asMessage()).willReturn(errorMessage);

    return exception;
  }

  private ContentNotFoundMessageException mockNotFoundException(String key, Object... args) {
    ContentNotFoundMessageException exception = mock(ContentNotFoundMessageException.class);
    Message errorMessage = new Message(key, args);
    given(exception.asMessage()).willReturn(errorMessage);

    return exception;
  }

  private ConvertToOrderDto generateConvertToOrderDto() {
    ConvertToOrderDto convertDto = new ConvertToOrderDto();
    convertDto.setSupplyingDepotId(UUID.randomUUID());
    convertDto.setRequisitionId(UUID.randomUUID());

    return convertDto;
  }

  private List<Requisition> generateRequisitionsWithMockedAccess(RequisitionStatus... statuses) {
    List<Requisition> requisitions = new ArrayList<>();

    for (RequisitionStatus status : statuses) {
      Requisition requisition = generateRequisition(status);
      requisitions.add(requisition);

      doNothing().when(permissionService).canViewRequisition(requisition.getId());
    }

    return requisitions;
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
        return null;
      }

      return collection
          .stream()
          .map(BuildRequisitionDtoAnswer::export)
          .collect(Collectors.toList());
    }
  }
}
