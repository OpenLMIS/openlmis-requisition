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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_DELETE;

import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;
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
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
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

import guru.nidi.ramltester.junit.RamlMatchers;

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
  private RequisitionRepository requisitionRepository;

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
  private PermissionService permissionService;

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

  @Autowired
  private MessageService messageService;

  // GET /api/requisitions/{id}

  @Before
  public void setUp() {
    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();
    mockUserAuthenticated();
  }

  @Test
  public void shouldGetChosenRequisition() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);
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
    assertNotNull(result);
    assertEquals(requisition.getId(), result.getId());

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetChosenRequisitionWhenUserHasNoRightForView() {
    // given
    UUID requisitionId = UUID.randomUUID();

    String missingPermission = REQUISITION_AUTHORIZE;

    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService)
        .canViewRequisition(any(UUID.class));

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
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
    given(requisitionRepository.findOne(any(UUID.class))).willReturn(null);
    doNothing().when(permissionService).canViewRequisition(any(UUID.class));

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

    doNothing().when(requisitionService).delete(requisition.getId());
    mockCanDeleteRequisition(requisition);

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
    verify(requisitionService).delete(requisition.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionWhenUserHasNoRightForDelete() {
    // given
    Requisition requisition = generateRequisition();
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

    String missingPermission = REQUISITION_DELETE;

    PermissionMessageException exception = mockPermissionException(missingPermission);
    doThrow(exception).when(permissionService).canDeleteRequisition(requisition.getId());

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(403)
        .body(MESSAGE, equalTo(getMessage(PERMISSION_ERROR_MESSAGE, missingPermission)));

    // then
    verify(requisitionRepository, never()).delete(any(UUID.class));
    verify(requisitionRepository, never()).delete(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonExistentRequisition() {
    // given
    UUID requisitionId = UUID.randomUUID();
    given(requisitionRepository.findOne(requisitionId)).willReturn(null);

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
    verify(requisitionRepository, never()).delete(any(UUID.class));
    verify(requisitionRepository, never()).delete(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionWithWrongStatus() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    UUID requisitionId = requisition.getId();
    doNothing().when(permissionService).canDeleteRequisition(requisitionId);

    String errorKey = MessageKeys.ERROR_DELETE_FAILED_WRONG_STATUS;

    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(requisitionService).delete(requisitionId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    verify(requisitionRepository, never()).delete(any(UUID.class));
    verify(requisitionRepository, never()).delete(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/search

  @Test
  public void shouldFindRequisitionsByParameters() {
    // given
    UUID periodId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID facilityId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();
    ZonedDateTime dateFrom = ZonedDateTime.now().minusDays(10);
    ZonedDateTime dateTo = ZonedDateTime.now().plusDays(10);
    Set<RequisitionStatus> statuses = Collections.singleton(RequisitionStatus.INITIATED);

    Requisition requisition = generateRequisition();
    List<Requisition> requisitions = Collections.singletonList(requisition);

    doNothing().when(permissionService).canViewRequisition(requisition.getId());
    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);
    given(requisitionService.searchRequisitions(
        eq(facilityId), eq(programId), any(ZonedDateTime.class), any(ZonedDateTime.class),
        eq(periodId), eq(supervisoryNodeId), eq(statuses), eq(null), any())
    ).willReturn(Pagination.getPage(requisitions, null));

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programId)
        .queryParam(PROCESSING_PERIOD, periodId)
        .queryParam(FACILITY, facilityId)
        .queryParam(SUPERVISORY_NODE, supervisoryNodeId)
        .queryParam(REQUISITION_STATUS, RequisitionStatus.INITIATED)
        .queryParam(INITIATED_DATE_FROM, dateFrom.minusDays(2).toString())
        .queryParam(INITIATED_DATE_TO, dateTo.plusDays(2).toString())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertNotNull(resultPage);
    assertEquals(1, resultPage.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldFindRequisitionsByMultipleStatuses() {
    // given
    RequisitionStatus[] searchedStatuses = {
        RequisitionStatus.SUBMITTED, RequisitionStatus.AUTHORIZED };
    Set<RequisitionStatus> statusSet = new HashSet<>(Arrays.asList(searchedStatuses));
    List<Requisition> requisitions = generateRequisitionsWithMockedAccess(searchedStatuses);

    given(requisitionService.searchRequisitions(
        eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), eq(statusSet), eq(null), any())
    ).willReturn(Pagination.getPage(requisitions, null));

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(REQUISITION_STATUS, searchedStatuses[0])
        .queryParam(REQUISITION_STATUS, searchedStatuses[1])
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertNotNull(resultPage);
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
        eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), any())
    ).willReturn(Pagination.getPage(requisitions, null));

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertNotNull(resultPage);
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
        eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), eq(null), any())
    ).willReturn(Pagination.getPage(requisitions, null));

    // when
    PageImplRepresentation resultPage = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertNotNull(resultPage);
    assertTrue(resultPage.getContent().isEmpty());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/{id}/submit

  @Test
  public void shouldSubmitValidRequisition() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.INITIATED));
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);
    given(orderableReferenceDataService.findAll()).willReturn(Collections.emptyList());

    doNothing().when(requisition).submit(any(), any(UUID.class));
    doNothing().when(permissionService).canViewRequisition(requisitionId);
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
    assertNotNull(result);
    assertEquals(requisitionId, result.getId());
    verify(requisition, atLeastOnce()).submit(any(), any(UUID.class));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWhenUserHasNoRightForSubmit() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.INITIATED));
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

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
    verify(requisition, never()).submit(anyCollectionOf(OrderableDto.class), any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitIfFacilityDoesNotSupportProgram() throws Exception {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.INITIATED));
    UUID requisitionId = requisition.getId();
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

    doNothing().when(permissionService).canSubmitRequisition(requisitionId);
    doNothing().when(requisitionValidator).validate(eq(requisition), any(BindingResult.class));

    String errorKey = MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(facilityId, programId);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionId)
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey)));

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
    // given
    doNothing().when(permissionService).canUpdateRequisition(any(UUID.class));

    String errorKey = MessageKeys.ERROR_SKIP_FAILED_WRONG_STATUS;
    ValidationMessageException exception = mockValidationException(errorKey);
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
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    verify(requisitionService, atLeastOnce()).skip(any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfItIsEmergency() {
    // given
    doNothing().when(permissionService).canUpdateRequisition(any(UUID.class));

    String errorKey = MessageKeys.ERROR_SKIP_FAILED_EMERGENCY;
    ValidationMessageException exception = mockValidationException(errorKey);
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
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    verify(requisitionService, atLeastOnce()).skip(any(UUID.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnNotFoundWhenSkippingNonExistentRequisition() {
    // given
    UUID requisitionId = UUID.randomUUID();
    given(requisitionRepository.findOne(requisitionId)).willReturn(null);

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
    Requisition requisition = spy(generateRequisition(RequisitionStatus.SUBMITTED));
    UUID requisitionId = requisition.getId();
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();
    SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
    given(supervisoryNode.getId()).willReturn(UUID.randomUUID());

    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);
    given(configurationSettingService.getBoolValue(any(String.class))).willReturn(false);
    given(supervisoryNodeReferenceDataService.findSupervisoryNode(programId, facilityId))
        .willReturn(supervisoryNode);
    given(orderableReferenceDataService.findAll()).willReturn(Collections.emptyList());
    doNothing().when(requisition).authorize(anyCollectionOf(OrderableDto.class), anyUuid());
    mockValidationSuccess();

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisitionId)
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
    Requisition requisition = spy(generateRequisition(RequisitionStatus.SUBMITTED));
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

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
    Requisition requisition = spy(generateRequisition(RequisitionStatus.SUBMITTED));
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
    Requisition requisition = spy(generateRequisition());
    UUID programId = requisition.getProgramId();
    UUID periodId = requisition.getProcessingPeriodId();
    UUID facilityId = requisition.getFacilityId();

    doNothing().when(permissionService).canInitRequisition(programId, facilityId);
    given(requisitionService.initiate(programId, facilityId, periodId, false))
        .willReturn(requisition);
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
    assertNotNull(result);
    assertEquals(requisition.getId(), result.getId());
    verify(requisitionService, atLeastOnce()).initiate(programId, facilityId, periodId, false);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenPeriodDoesNotExist() {
    // given
    Requisition requisition = generateRequisition();
    UUID programId = requisition.getProgramId();
    UUID periodId = requisition.getProcessingPeriodId();
    UUID facilityId = requisition.getFacilityId();

    ValidationMessageException err =
        mockValidationException(MessageKeys.ERROR_INCORRECT_SUGGESTED_PERIOD);

    doNothing().when(permissionService).canInitRequisition(programId, facilityId);
    given(requisitionService.initiate(programId, facilityId, periodId, false)).willThrow(err);
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
    verify(requisitionRepository, never()).save(any(Requisition.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenFacilityDoesNotSupportProgram() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.AUTHORIZED));
    UUID requisitionId = requisition.getId();
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();

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
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityId)
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    verify(requisition, never()).initiate(any(), any(), any(), anyInt(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateRequisitionWhenUserHasNoRight() {
    // given
    UUID programId = UUID.randomUUID();
    UUID facilityId = UUID.randomUUID();

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
    verify(requisitionService, never())
        .initiate(any(UUID.class), any(UUID.class), any(UUID.class), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitions/{id}/approve

  @Test
  public void shouldApproveSubmittedRequisitionWhenSkippedAuthorization() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.SUBMITTED));
    UUID requisitionId = requisition.getId();
    given(requisitionRepository.findOne(requisitionId)).willReturn(requisition);

    UUID supervisoryNodeId = requisition.getSupervisoryNodeId();
    given(supervisoryNodeReferenceDataService.findOne(supervisoryNodeId))
        .willReturn(new SupervisoryNodeDto());

    given(orderableReferenceDataService.findAll()).willReturn(Collections.emptyList());
    given(configurationSettingService.getBoolValue("skipAuthorization")).willReturn(true);
    doNothing().when(permissionService).canApproveRequisition(requisitionId);
    doNothing().when(requisition).approve(any(UUID.class), anyCollectionOf(OrderableDto.class));
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
    assertNotNull(result);
    assertEquals(requisitionId, result.getId());
    verify(requisition, atLeastOnce())
        .approve(any(UUID.class), anyCollectionOf(OrderableDto.class));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveRequisitionWhenUserHasNoRightForApprove() {
    // given
    Requisition requisition = spy(generateRequisition());
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
    verify(requisition, never()).approve(any(UUID.class), anyCollectionOf(OrderableDto.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveWhenFacilityDoesNotSupportProgram() {
    // given
    Requisition requisition = spy(generateRequisition(RequisitionStatus.AUTHORIZED));
    UUID requisitionId = requisition.getId();
    UUID facilityId = requisition.getFacilityId();
    UUID programId = requisition.getProgramId();

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
        .post(APPROVE_URL)
        .then()
        .statusCode(400)
        .body(MESSAGE, equalTo(getMessage(errorKey)));

    // then
    verify(requisition, never()).approve(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/submitted

  @Test
  public void shouldGetSubmittedRequisitions() {
    // given
    Requisition[] requisitions = { generateRequisition(), generateRequisition() };
    given(requisitionService.searchRequisitions(
        anySetOf(RequisitionStatus.class), any(Pageable.class)))
        .willReturn(Pagination.getPage(Arrays.asList(requisitions), null));

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
        anySetOf(RequisitionStatus.class), any(Pageable.class)))
        .willReturn(Pagination.getPage(Arrays.asList(requisitions), null));

    PermissionMessageException exception = mock(PermissionMessageException.class);
    doThrow(exception).when(permissionService).canViewRequisition(availableRequisition.getId());

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
        eq(EnumSet.of(RequisitionStatus.SUBMITTED)), any(Pageable.class)))
        .willReturn(Pagination.getPage(requisitions, null));

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

    doThrow(exception).when(permissionService)
        .canInitOrAuthorizeRequisition(any(UUID.class), any(UUID.class));

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
    verify(periodService, never()).getPeriods(any(UUID.class), any(UUID.class), anyBoolean());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions/requisitionsForApproval

  @Test
  public void shouldGetRequisitionsForApprovalForSpecificUser() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    Set<Requisition> requisitions = Collections.singleton(requisition);

    given(requisitionService.getRequisitionsForApproval(any(UUID.class))).willReturn(requisitions);

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
    assertNotNull(result);
    assertEquals(1, result.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitions

  @Test
  public void shouldGetApprovedRequisitionsWithSortByAscendingFilterByAndPaging() {
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
    Boolean descending = false;
    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        eq(filterValue), eq(filterBy), eq(sortBy), eq(descending), any(Pageable.class),
        eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(Collections.singletonList(requisition), null));

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", filterValue)
        .queryParam("filterBy", filterBy)
        .queryParam("sortBy", sortBy)
        .queryParam("descending", descending.toString())
        .queryParam("page", page)
        .queryParam("size", size)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertNotNull(response);
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetApprovedRequisitionsWithSortByDescendingFilterByAndPaging() {
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
    Boolean descending = true;
    int size = 10;
    int page = 0;

    given(requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        eq(filterValue), eq(filterBy), eq(sortBy), eq(descending), any(Pageable.class),
        eq(managedFacilitiesIds)))
        .willReturn(Pagination.getPage(Collections.singletonList(requisition), null));

    // when
    PageImplRepresentation response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", filterValue)
        .queryParam("filterBy", filterBy)
        .queryParam("sortBy", sortBy)
        .queryParam("descending", descending.toString())
        .queryParam("page", page)
        .queryParam("size", size)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertNotNull(response);
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetApprovedRequisitionsIfUserHasNoFulfillmentRightsForFacility() {
    // given
    RightDto right = new RightDto();
    right.setId(UUID.randomUUID());
    given(authenticationHelper.getRight(RightName.ORDERS_EDIT)).willReturn(right);

    FacilityDto facility = new FacilityDto();
    facility.setId(UUID.randomUUID());

    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(Collections.emptySet());

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
    assertNotNull(response);
    assertEquals(0, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetApprovedRequisitionsWithUserFulfillmentRights() {
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
    assertNotNull(response);
    assertEquals(1, response.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitions/convertToOrder

  @Test
  public void shouldConvertRequisitionToOrder() {
    // given
    ConvertToOrderDto convertDto = new ConvertToOrderDto();
    convertDto.setSupplyingDepotId(UUID.randomUUID());
    convertDto.setRequisitionId(UUID.randomUUID());
    List<ConvertToOrderDto> requisitions = Collections.singletonList(convertDto);

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
    ConvertToOrderDto convertDto = new ConvertToOrderDto();
    convertDto.setSupplyingDepotId(UUID.randomUUID());
    convertDto.setRequisitionId(UUID.randomUUID());
    List<ConvertToOrderDto> requisitions = Collections.singletonList(convertDto);

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

  private void mockCanDeleteRequisition(Requisition requisition) {
    doNothing().when(permissionService).canDeleteRequisition(requisition.getId());
  }

  private void mockValidationSuccess() {
    given(statusMessageRepository.save(any(StatusMessage.class))).willReturn(null);
    doNothing().when(requisitionStatusProcessor).statusChange(any(Requisition.class));

    doNothing().when(requisitionValidator).validate(any(Object.class), any(Errors.class));
    doNothing().when(facilitySupportsProgramHelper)
        .checkIfFacilitySupportsProgram(any(UUID.class), any(UUID.class));
  }

  private void mockRequisitionDtoBuilderResponses() {
    given(requisitionDtoBuilder.build(any(Requisition.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.build(anyListOf(Requisition.class)))
        .willAnswer(new BuildListOfRequisitionDtosAnswer());
  }

  private void mockRepositorySaveAnswer() {
    given(requisitionRepository.save(any(Requisition.class))).willAnswer(new SaveAnswer<>());
  }

  private ValidationMessageException mockValidationException(String key, Object... args) {
    ValidationMessageException exception = mock(ValidationMessageException.class);
    Message errorMessage = new Message(key, args);
    given(exception.asMessage()).willReturn(errorMessage);

    return exception;
  }

  private ContentNotFoundMessageException mockNotFoundException(String key, Object... args) {
    ContentNotFoundMessageException exception = mock(ContentNotFoundMessageException.class);
    Message errorMessage = new Message(key, args);
    given(exception.asMessage()).willReturn(errorMessage);

    return exception;
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

      dto.setTemplate(requisition.getTemplate());
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
