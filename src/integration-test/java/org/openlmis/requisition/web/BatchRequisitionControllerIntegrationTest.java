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

import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Mockito.doReturn;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.jayway.restassured.response.Response;
import com.jayway.restassured.specification.RequestSpecification;

import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ApproveRequisitionDto;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class BatchRequisitionControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String RETRIEVE_ALL = "retrieveAll";
  private static final String APPROVE_ALL = "approveAll";
  private static final String SAVE_ALL = "saveAll";
  private static final String ID = "id";

  @MockBean
  private RequisitionService requisitionService;

  @MockBean
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @MockBean
  private RequisitionValidator validator;

  @MockBean
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @MockBean
  private OrderableReferenceDataService orderableReferenceDataService;

  @MockBean
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @MockBean
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @MockBean
  private RequisitionVersionValidator requisitionVersionValidator;

  @MockBean(name = "programReferenceDataService")
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private ProgramDto program;

  private List<Requisition> requisitions;
  private List<ApproveRequisitionDto> approveRequisitions;
  private List<UUID> requisitionIds;

  @Before
  public void setUp() {
    mockUserAuthenticated();

    mockRepositorySaveAnswer();
    mockRequisitionDtoBuilderResponses();

    requisitions = Lists.newArrayList(
        generateRequisition(RequisitionStatus.AUTHORIZED),
        generateRequisition(RequisitionStatus.AUTHORIZED),
        generateRequisition(RequisitionStatus.AUTHORIZED)
    );

    requisitionIds = requisitions
        .stream()
        .map(BaseEntity::getId)
        .collect(Collectors.toList());

    approveRequisitions = requisitions
        .stream()
        .map(requisition -> requisitionDtoBuilder.build(requisition))
        .map(ApproveRequisitionDto::new)
        .collect(Collectors.toList());

    doReturn(requisitions)
        .when(requisitionRepository).findAll(requisitionIds);

    doReturn(ValidationResult.success())
        .when(requisitionVersionValidator)
        .validateRequisitionTimestamps(any(Requisition.class), any(Requisition.class));

    doReturn(Collections.emptyList())
        .when(orderableReferenceDataService)
        .findByIds(anySetOf(UUID.class));

    doReturn(false)
        .when(program)
        .getEnableDatePhysicalStockCountCompleted();

    doReturn(program)
        .when(programReferenceDataService)
        .findOne(any(UUID.class));
  }

  // GET /api/requisitions?retrieveAll&id={}&id={}&id={}

  @Test
  public void shouldRetrieveAll() throws Exception {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(permissionService).canViewRequisition(requisition)
    );

    Response response = get(RETRIEVE_ALL, requisitionIds);
    checkResponseBody(response);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToView() throws Exception {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(permissionService).canViewRequisition(requisition)
    );

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_VIEW))
        .when(permissionService).canViewRequisition(requisitions.get(0));

    Response response = get(RETRIEVE_ALL, requisitionIds);
    checkErrorResponseBody(response, 400);
  }

  // POST /api/requisitions?approveAll&id={}&id={}&id={}

  @Test
  public void shouldApproveAll() throws Exception {
    UUID userId = authenticationHelper.getCurrentUser().getId();

    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanApproveRequisition(requisition, requisition.getId(), userId)
    );

    Response response = post(APPROVE_ALL, requisitionIds);
    checkResponseBody(response);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToApprove() throws Exception {
    UUID userId = authenticationHelper.getCurrentUser().getId();

    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanApproveRequisition(requisition, requisition.getId(), userId)
    );

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_APPROVE))
        .when(requisitionService)
        .validateCanApproveRequisition(requisitions.get(0), requisitions.get(0).getId(), userId);

    Response response = post(APPROVE_ALL, requisitionIds);
    checkErrorResponseBody(response, 400);
  }

  // PUT /api/requisitions?saveAll

  @Test
  public void shouldSaveAll() throws Exception {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanSaveRequisition(requisition.getId())
    );

    Response response = put(SAVE_ALL, approveRequisitions);
    checkResponseBody(response);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToUpdate() throws Exception {
    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanSaveRequisition(requisition.getId())
    );

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_APPROVE))
        .when(requisitionService).validateCanSaveRequisition(requisitions.get(0).getId());

    Response response = put(SAVE_ALL, approveRequisitions);
    checkErrorResponseBody(response, 400);
  }

  private void checkResponseBody(Response response) throws IOException {
    String jsonString = response
        .then()
        .statusCode(200)
        .extract()
        .asString();

    JsonNode json = objectMapper.readTree(jsonString);
    ArrayNode requisitionErrors = (ArrayNode) json.get("requisitionErrors");

    assertThat(requisitionErrors.size(), equalTo(0));

    ArrayNode requisitionDtos = (ArrayNode) json.get("requisitionDtos");

    List<UUID> retrieved = getIds(requisitionDtos);

    assertThat(retrieved, hasItems(requisitionIds.toArray(new UUID[requisitionIds.size()])));
  }

  private void checkErrorResponseBody(Response response, int statusCode) throws IOException {
    String jsonString = response
        .then()
        .statusCode(statusCode)
        .extract()
        .asString();

    JsonNode json = objectMapper.readTree(jsonString);
    ArrayNode requisitionErrors = (ArrayNode) json.get("requisitionErrors");

    assertThat(requisitionErrors.size(), equalTo(1));
    assertThat(
        requisitionErrors.get(0).get("requisitionId").asText(),
        equalTo(requisitions.get(0).getId().toString())
    );
    assertThat(
        requisitionErrors.get(0).get("errorMessage").get("messageKey").asText(),
        equalTo(ERROR_NO_FOLLOWING_PERMISSION)
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

  private void mockRepositorySaveAnswer() {
    given(requisitionRepository.save(any(Requisition.class))).willAnswer(new SaveAnswer<>());
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
      if (requisition.getProcessingPeriodId() != null) {
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
          .map(BatchRequisitionControllerIntegrationTest.BuildRequisitionDtoAnswer::export)
          .collect(Collectors.toList());
    }
  }

}
