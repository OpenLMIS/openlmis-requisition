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

import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyList;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anySet;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.internal.verification.VerificationModeFactory.atLeastOnce;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.jayway.restassured.response.Response;
import com.jayway.restassured.specification.RequestSpecification;
import guru.nidi.ramltester.junit.RamlMatchers;
import java.io.IOException;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.VersionEntityReference;
import org.openlmis.requisition.dto.ApproveRequisitionDto;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReleasableRequisitionBatchDto;
import org.openlmis.requisition.dto.ReleasableRequisitionDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.DetailedRoleAssignmentDtoDataBuilder;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.ProcessingPeriodDtoDataBuilder;
import org.openlmis.requisition.testutils.ReleasableRequisitionBatchDtoDataBuilder;
import org.openlmis.requisition.testutils.ReleasableRequisitionDtoDataBuilder;
import org.openlmis.requisition.testutils.RightDtoDataBuilder;
import org.openlmis.requisition.testutils.RoleDtoDataBuilder;
import org.openlmis.requisition.testutils.SupervisoryNodeDtoDataBuilder;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.validate.RequisitionValidationTestUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class BatchRequisitionControllerIntegrationTest extends BaseRequisitionWebIntegrationTest {
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String BATCH_RELEASES_URL = RESOURCE_URL + "/batchReleases";
  private static final String RETRIEVE_ALL = "retrieveAll";
  private static final String APPROVE_ALL = "approveAll";
  private static final String SAVE_ALL = "saveAll";
  private static final String ID = "id";

  private ProgramDto program = DtoGenerator.of(ProgramDto.class);

  private List<Requisition> requisitions;
  private List<ApproveRequisitionDto> approveRequisitions;
  private List<UUID> requisitionIds;
  private Map<VersionIdentityDto, OrderableDto> orderables;
  private Map<VersionIdentityDto, ApprovedProductDto> ftaps;
  private List<String> permissionStrings;
  private UserDto user;
  private RightDto approveRight = new RightDtoDataBuilder().buildAsDto();

  @Before
  public void setUp() {
    super.setUp();

    user = mockUserAuthenticated();

    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();
    mockStockEventServiceResponses();

    doReturn(program)
        .when(programReferenceDataService)
        .findOne(any(UUID.class));

    requisitions = Lists.newArrayList(
        generateRequisition(RequisitionStatus.AUTHORIZED),
        generateRequisition(RequisitionStatus.AUTHORIZED),
        generateRequisition(RequisitionStatus.AUTHORIZED)
    );
    requisitions.forEach(r -> r.setSupervisoryNodeId(UUID.randomUUID()));

    doReturn(requisitions)
        .when(requisitionRepository)
        .readDistinctByIdIn(anySetOf(UUID.class));

    orderables = Maps.newHashMap();
    ftaps = Maps.newHashMap();

    for (Requisition requisition : requisitions) {
      requisition
          .getRequisitionLineItems()
          .stream()
          .peek(line -> line.setApprovedQuantity(10))
          .map(line -> new OrderableDtoDataBuilder()
              .withId(line.getOrderable().getId())
              .withVersionNumber(line.getOrderable().getVersionNumber())
              .withProgramOrderable(requisition.getProgramId(), true)
              .buildAsDto())
          .forEach(orderable -> orderables.put(orderable.getIdentity(), orderable));

      requisition
          .getRequisitionLineItems()
          .stream()
          .map(line -> new ApprovedProductDtoDataBuilder()
              .withId(line.getFacilityTypeApprovedProduct().getId())
              .withVersionNumber(line.getFacilityTypeApprovedProduct().getVersionNumber())
              .buildAsDto())
          .forEach(ftap -> ftaps.put(ftap.getIdentity(), ftap));
    }

    permissionStrings = requisitions
        .stream()
        .map(req -> String.format("%s|%s|%s", REQUISITION_APPROVE,
            req.getFacilityId(), req.getProgramId()))
        .collect(Collectors.toList());

    doReturn(permissionStrings)
        .when(userReferenceDataService).getPermissionStrings(user.getId());

    doReturn(approveRight).when(rightReferenceDataService).findRight(REQUISITION_APPROVE);
    doReturn(roleAssignmentsAtRequisitionNodes()).when(userRoleAssignmentsReferenceDataService)
        .getRoleAssignments(user.getId());
    doReturn(Collections.emptyList()).when(supervisoryNodeReferenceDataService)
        .findByFacility(any(UUID.class));

    requisitionIds = requisitions
        .stream()
        .map(BaseEntity::getId)
        .collect(Collectors.toList());

    approveRequisitions = requisitions
        .stream()
        .map(requisitionDtoBuilder::build)
        .map(req -> new ApproveRequisitionDto(req, req.getProgramId(), orderables))
        .collect(Collectors.toList());

    doReturn(requisitions)
        .when(requisitionRepository).findAllById(requisitionIds);

    doReturn(ValidationResult.success())
        .when(requisitionVersionValidator)
        .validateRequisitionTimestamps(any(ZonedDateTime.class), any(Requisition.class));

    doReturn(Lists.newArrayList(orderables.values()))
        .when(orderableReferenceDataService)
        .findByIdentities(anySetOf(VersionEntityReference.class));

    doReturn(Lists.newArrayList(ftaps.values()))
        .when(facilityTypeApprovedProductReferenceDataService)
        .findByIdentities(anySet());

    program.setEnableDatePhysicalStockCountCompleted(false);

    List<FacilityDto> facilityList = requisitions.stream()
        .map(r -> {
          FacilityDto facilityDto = new FacilityDtoDataBuilder()
              .withId(r.getFacilityId())
              .buildAsDto();

          when(facilityReferenceDataService.findOne(r.getFacilityId())).thenReturn(facilityDto);

          return facilityDto;
        })
        .collect(Collectors.toList());
    doReturn(facilityList)
        .when(facilityReferenceDataService)
        .search(anySetOf(UUID.class));

    List<ProcessingPeriodDto> periods = requisitions
        .stream()
        .map(r -> new ProcessingPeriodDtoDataBuilder()
            .withId(r.getProcessingPeriodId())
            .buildAsDto())
        .collect(Collectors.toList());

    doReturn(periods)
        .when(periodReferenceDataService)
        .search(anySetOf(UUID.class));

    mockSearchSupervisoryNodeByProgramAndFacility();
  }

  @Test
  public void shouldRetrieveAll() throws IOException {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(permissionService).canViewRequisition(requisition)
    );

    Response response = get(RETRIEVE_ALL, requisitionIds);
    checkResponseBody(response);

    ArgumentCaptor<ProcessingPeriodDto> periodCaptor =
        ArgumentCaptor.forClass(ProcessingPeriodDto.class);
    ArgumentCaptor<FacilityDto> facilityCaptor = ArgumentCaptor.forClass(FacilityDto.class);

    requisitions.forEach(req -> {
      verify(requisitionDtoBuilder)
          .buildBatch(eq(req), facilityCaptor.capture(),
              eq(orderables), eq(ftaps), periodCaptor.capture());

      assertEquals(req.getProcessingPeriodId(), periodCaptor.getValue().getId());
      assertEquals(req.getFacilityId(), facilityCaptor.getValue().getId());
    });

  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToView() throws IOException {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(permissionService).canViewRequisition(requisition)
    );

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_VIEW))
        .when(permissionService).canViewRequisition(requisitions.get(0));

    Response response = get(RETRIEVE_ALL, requisitionIds);
    checkPermissionErrorResponseBody(response, 400);
  }

  @Test
  public void shouldApproveAll() throws IOException {
    mockRequisitionValidatonsAndStubRepository();

    Response response = post(APPROVE_ALL, requisitionIds);
    checkResponseBody(response);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToApprove() throws IOException {
    permissionStrings = permissionStrings.stream().skip(1).collect(Collectors.toList());
    doReturn(permissionStrings)
        .when(userReferenceDataService)
        .getPermissionStrings(user.getId());

    mockRequisitionValidatonsAndStubRepository();

    Response response = post(APPROVE_ALL, requisitionIds);
    checkPermissionErrorResponseBody(response, 400);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoApproveRoleAtRequisitionSupervisoryNode()
      throws IOException {
    List<DetailedRoleAssignmentDto> assignments = roleAssignmentsAtRequisitionNodes();
    assignments.get(0).setSupervisoryNodeId(UUID.randomUUID());
    doReturn(assignments).when(userRoleAssignmentsReferenceDataService)
        .getRoleAssignments(user.getId());
    mockRequisitionValidatonsAndStubRepository();

    Response response = post(APPROVE_ALL, requisitionIds);
    checkValidationErrorResponseBody(response, 400,
        requisitionErrors -> requisitionErrors.get(0)
            .get("errorMessage")
            .get("messageKey")
            .asText(),
        ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION);
  }

  @Test
  public void shouldApproveWhenUserHasApproveRoleAtNodeSupervisingTheFacility()
      throws IOException {
    SupervisoryNodeDto facilityNode = new SupervisoryNodeDtoDataBuilder().buildAsDto();
    List<DetailedRoleAssignmentDto> assignments = roleAssignmentsAtRequisitionNodes();
    assignments.get(0).setSupervisoryNodeId(facilityNode.getId());
    doReturn(assignments).when(userRoleAssignmentsReferenceDataService)
        .getRoleAssignments(user.getId());
    doReturn(singletonList(facilityNode)).when(supervisoryNodeReferenceDataService)
        .findByFacility(requisitions.get(0).getFacilityId());
    mockRequisitionValidatonsAndStubRepository();

    Response response = post(APPROVE_ALL, requisitionIds);
    checkResponseBody(response);
  }

  @Test
  public void shouldFetchApproverRightsOncePerBatchAndNodesOncePerFacility()
      throws IOException {
    mockRequisitionValidatonsAndStubRepository();

    Response response = post(APPROVE_ALL, requisitionIds);
    checkResponseBody(response);

    verify(rightReferenceDataService).findRight(REQUISITION_APPROVE);
    verify(userRoleAssignmentsReferenceDataService).getRoleAssignments(user.getId());
    requisitions.forEach(r -> verify(supervisoryNodeReferenceDataService)
        .findByFacility(r.getFacilityId()));
    verifyNoMoreInteractions(rightReferenceDataService, userRoleAssignmentsReferenceDataService);
  }

  @Test
  public void shouldHaveErrorIfValidationFails() throws IOException {
    requisitions = mockRequisitionValidatonsAndStubRepository();

    doReturn(ValidationResult.fieldErrors(ImmutableMap.of("someField", new Message("some-key"))))
        .when(requisitions.get(0))
        .validateCanChangeStatus(any(LocalDate.class), anyBoolean(), anyMap(), anyMap());

    Response response = post(APPROVE_ALL, requisitionIds);
    checkValidationErrorResponseBody(response, 400,
        requisitionErrors -> requisitionErrors.get(0)
            .get("fieldErrors")
            .get("someField")
            .get("messageKey")
            .asText(),
        "some-key");
  }

  @Test
  public void shouldSubmitStockEventForEachApprovedRequisition() {
    mockRequisitionValidatonsAndStubRepository();
    mockFinalApproval();

    post(APPROVE_ALL, requisitionIds);

    verify(stockEventStockManagementService, times(3)).submit(any(StockEventDto.class));
  }

  @Test
  public void shouldNotSubmitStockEventForRequisitionTheBatchDidNotApprove() {
    mockRequisitionValidatonsAndStubRepository().get(0).setStatus(RequisitionStatus.APPROVED);
    mockFinalApproval();

    post(APPROVE_ALL, requisitionIds);

    verify(stockEventStockManagementService, times(2)).submit(any(StockEventDto.class));
  }

  @Test
  public void shouldNotSubmitStockEventForStockBasedTemplate() {
    useTemplate(mockRequisitionValidatonsAndStubRepository(), templateBuilder()
        .withPopulateStockOnHandFromStockCards()
        .build());
    mockFinalApproval();

    post(APPROVE_ALL, requisitionIds);

    verify(stockEventStockManagementService, never()).submit(any(StockEventDto.class));
  }

  @Test
  public void shouldNotSubmitStockEventWhenPatientsTabIsEnabled() {
    useTemplate(mockRequisitionValidatonsAndStubRepository(), templateBuilder()
        .withPatientsTabEnabled()
        .build());
    mockFinalApproval();

    post(APPROVE_ALL, requisitionIds);

    verify(stockEventStockManagementService, never()).submit(any(StockEventDto.class));
  }

  // PUT /api/requisitions?saveAll

  @Test
  public void shouldSaveAll() throws IOException {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanSaveRequisition(requisition.getId())
    );

    Response response = put(SAVE_ALL, approveRequisitions);
    checkResponseBody(response);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToUpdate() throws IOException {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanSaveRequisition(requisition.getId())
    );

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION,
        PermissionService.REQUISITION_APPROVE))
        .when(requisitionService).validateCanSaveRequisition(requisitions.get(0).getId());

    Response response = put(SAVE_ALL, approveRequisitions);
    checkPermissionErrorResponseBody(response, 400);
  }

  // POST /api/requisitions/batchReleases

  @Test
  public void shouldConvertRequisitionToOrder() {
    // given
    List<ReleasableRequisitionDto> requisitions = singletonList(generateReleasableRequisitionDto());
    ReleasableRequisitionBatchDto releaseDto = generateReleaseRequisitionDto(requisitions);
    releaseDto.setCreateOrder(true);

    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());

    doReturn(new ArrayList<>())
        .when(requisitionService).convertToOrder(any(), any());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(releaseDto)
        .when()
        .post(BATCH_RELEASES_URL)
        .then()
        .statusCode(201);

    // then
    verify(requisitionService, atLeastOnce()).convertToOrder(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturn403StatusWhenUserHasNoPermission() {
    // given
    List<ReleasableRequisitionDto> requisitions = singletonList(generateReleasableRequisitionDto());
    ReleasableRequisitionBatchDto releaseDto = generateReleaseRequisitionDto(requisitions);
    releaseDto.setCreateOrder(false);

    doReturn(ValidationResult.noPermission("No permission", "no permission"))
        .when(permissionService).canConvertToOrder(anyList());
    doReturn(new ArrayList<>())
        .when(requisitionService).convertToOrder(any(), any());
    doReturn(new ArrayList<>())
        .when(requisitionService).releaseWithoutOrder(any());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(releaseDto)
        .when()
        .post(BATCH_RELEASES_URL)
        .then()
        .statusCode(403);

    // then
    verify(requisitionService, never()).convertToOrder(any(), any());
    verify(requisitionService, never()).releaseWithoutOrder(any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotConvertRequisitionToOrderWhenConvertToOrderDtoIsInvalid() {
    // given
    List<ReleasableRequisitionDto> requisitions = singletonList(generateReleasableRequisitionDto());
    ReleasableRequisitionBatchDto releaseDto = generateReleaseRequisitionDto(requisitions);
    releaseDto.setCreateOrder(true);

    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());

    String errorKey = MessageKeys.ERROR_CONVERTING_REQUISITION_TO_ORDER;
    ValidationMessageException exception = mockValidationException(errorKey);
    doThrow(exception).when(requisitionService).convertToOrder(any(), any());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(releaseDto)
        .when()
        .post(BATCH_RELEASES_URL)
        .then()
        .statusCode(400);

    // then
    verify(requisitionService, atLeastOnce()).convertToOrder(any(), any());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private ReleasableRequisitionBatchDto generateReleaseRequisitionDto(
      List<ReleasableRequisitionDto> requisitions) {
    ReleasableRequisitionBatchDto releaseDto = new ReleasableRequisitionBatchDtoDataBuilder()
        .withRequisitionsToRelease(requisitions)
        .buildAsDto();
    return releaseDto;
  }

  private ReleasableRequisitionDto generateReleasableRequisitionDto() {
    return new ReleasableRequisitionDtoDataBuilder()
        .buildAsDto();
  }

  private void checkResponseBody(Response response) throws IOException {
    String jsonString = response
        .then()
        .statusCode(200)
        .extract()
        .asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    JsonNode json = objectMapper.readTree(jsonString);
    ArrayNode requisitionErrors = (ArrayNode) json.get("requisitionErrors");

    assertThat(requisitionErrors.size(), equalTo(0));

    ArrayNode requisitionDtos = (ArrayNode) json.get("requisitionDtos");

    List<UUID> retrieved = getIds(requisitionDtos);

    assertThat(retrieved, hasItems(requisitionIds.toArray(new UUID[requisitionIds.size()])));
  }

  private void checkPermissionErrorResponseBody(Response response, int statusCode)
      throws IOException {
    checkValidationErrorResponseBody(response, statusCode,
        requisitionErrors -> requisitionErrors.get(0)
            .get("errorMessage")
            .get("messageKey")
            .asText(),
        ERROR_NO_FOLLOWING_PERMISSION);
  }

  private void checkValidationErrorResponseBody(Response response, int statusCode,
                                                Function<ArrayNode, String> fieldToAssert,
                                                String expectedKey) throws IOException {
    String jsonString = response
        .then()
        .statusCode(statusCode)
        .extract()
        .asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    JsonNode json = objectMapper.readTree(jsonString);
    final ArrayNode requisitionErrors = (ArrayNode) json.get("requisitionErrors");

    assertThat(requisitionErrors.size(), equalTo(1));
    assertThat(
        requisitionErrors.get(0).get("requisitionId").asText(),
        equalTo(requisitions.get(0).getId().toString())
    );
    assertThat(
        fieldToAssert.apply(requisitionErrors),
        equalTo(expectedKey)
    );

    ArrayNode requisitionDtos = (ArrayNode) json.get("requisitionDtos");

    List<UUID> retrieved = getIds(requisitionDtos);

    assertThat(retrieved, allOf(
        not(hasItem(requisitions.get(0).getId())),
        hasItem(requisitions.get(1).getId()),
        hasItem(requisitions.get(2).getId())
    ));
  }

  private List<UUID> getIds(ArrayNode requisitionDtos) {
    List<UUID> result = Lists.newArrayList();
    requisitionDtos.forEach(node -> result.add(UUID.fromString(node.get("id").asText())));

    return result;
  }

  private Response put(String param, List<ApproveRequisitionDto> body) {
    return startRequest(param)
        .body(body)
        .put(RESOURCE_URL);
  }

  private Response get(String param, List<UUID> ids) {
    RequestSpecification specification = startRequest(param);

    for (UUID id : ids) {
      specification = specification.queryParam(ID, id);
    }

    return specification.get(RESOURCE_URL);
  }

  private Response post(String param, List<UUID> ids) {
    RequestSpecification specification = startRequest(param);

    for (UUID id : ids) {
      specification = specification.queryParam(ID, id);
    }

    return specification.post(RESOURCE_URL);
  }

  private RequestSpecification startRequest(String param) {
    return restAssured
        .given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .queryParam(param, "");
  }

  private RequisitionTemplateDataBuilder templateBuilder() {
    return new RequisitionTemplateDataBuilder()
        .withColumns(RequisitionValidationTestUtils.initiateColumns());
  }

  private void useTemplate(List<Requisition> spies, RequisitionTemplate template) {
    spies.forEach(requisition -> requisition.setTemplate(template));
  }

  private void mockFinalApproval() {
    doAnswer(invocation -> {
      invocation.getArgument(3, Requisition.class).setStatus(RequisitionStatus.APPROVED);
      return null;
    }).when(requisitionService).doApprove(any(), any(), anyMap(), any(Requisition.class),
        anyList(), any(), any());
  }

  private List<DetailedRoleAssignmentDto> roleAssignmentsAtRequisitionNodes() {
    return requisitions
        .stream()
        .map(r -> {
          RoleDto role = new RoleDtoDataBuilder().buildAsDto();
          role.setRights(Sets.newHashSet(approveRight));
          DetailedRoleAssignmentDto assignment =
              new DetailedRoleAssignmentDtoDataBuilder().buildAsDto();
          assignment.setRole(role);
          assignment.setProgramId(r.getProgramId());
          assignment.setSupervisoryNodeId(r.getSupervisoryNodeId());
          return assignment;
        })
        .collect(Collectors.toList());
  }

  private List<Requisition> mockRequisitionValidatonsAndStubRepository() {
    List<Requisition> requisitionSpies = new ArrayList<>();
    requisitions.forEach(requisition -> {
      Requisition spy = spy(requisition);
      requisitionSpies.add(spy);
      when(spy.validateCanChangeStatus(any(LocalDate.class), anyBoolean(), anyMap(), anyMap()))
          .thenReturn(ValidationResult.success());
    });

    given(requisitionRepository.readDistinctByIdIn(requisitionIds)).willReturn(requisitionSpies);

    return requisitionSpies;
  }

}
