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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.google.common.collect.Sets;
import com.jayway.restassured.response.Response;
import com.jayway.restassured.specification.RequestSpecification;
import guru.nidi.ramltester.junit.RamlMatchers;
import java.io.IOException;
import java.time.LocalDate;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.lang3.RandomStringUtils;
import org.assertj.core.util.Lists;
import org.assertj.core.util.Maps;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ApproveRequisitionDto;
import org.openlmis.requisition.dto.BasicOrderableDto;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

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
  private DatePhysicalStockCountCompletedEnabledPredicate predicate;

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

  @MockBean
  private StockEventStockManagementService stockEventStockManagementService;

  @MockBean
  private StockEventBuilder stockEventBuilder;

  @MockBean(name = "facilityReferenceDataService")
  private FacilityReferenceDataService facilityReferenceDataService;

  @MockBean
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  private ProgramDto program = DtoGenerator.of(ProgramDto.class);

  private List<Requisition> requisitions;
  private List<ApproveRequisitionDto> approveRequisitions;
  private List<UUID> requisitionIds;
  private Map<UUID, OrderableDto> orderablesMap;

  @Before
  public void setUp() {
    mockUserAuthenticated();

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

    List<OrderableDto> orderables =
        Collections.singletonList(generateOrderable(LINE_ITEM_PRODUCT_ID, requisitions));
    doReturn(orderables)
        .when(orderableReferenceDataService)
        .findByIds(Collections.singleton(LINE_ITEM_PRODUCT_ID));

    orderablesMap = orderables.stream()
        .collect(Collectors.toMap(BasicOrderableDto::getId, orderable -> orderable));

    program.setEnableDatePhysicalStockCountCompleted(false);

    List<FacilityDto> facilityList = requisitions.stream()
        .map(r -> {
          FacilityDto facilityDto = new FacilityDto();
          facilityDto.setId(r.getFacilityId());

          when(facilityReferenceDataService.findOne(r.getFacilityId())).thenReturn(facilityDto);

          return facilityDto;
        })
        .collect(Collectors.toList());
    doReturn(facilityList)
        .when(facilityReferenceDataService)
        .search(requisitions.stream().map(Requisition::getFacilityId).collect(Collectors.toSet()));
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
              eq(orderablesMap), periodCaptor.capture());

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
    UUID userId = authenticationHelper.getCurrentUser().getId();

    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanApproveRequisition(refEq(requisition), eq(userId))
    );

    requisitions.forEach(this::spyRequisitionAndStubRepository);

    Response response = post(APPROVE_ALL, requisitionIds);
    checkResponseBody(response);
  }

  @Test
  public void shouldHaveErrorIfUserHasNoRightToApprove() throws IOException {
    UUID userId = authenticationHelper.getCurrentUser().getId();

    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanApproveRequisition(refEq(requisition), eq(userId))
    );

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_APPROVE))
        .when(requisitionService)
        .validateCanApproveRequisition(refEq(requisitions.get(0)), eq(userId));

    requisitions.forEach(this::spyRequisitionAndStubRepository);

    Response response = post(APPROVE_ALL, requisitionIds);
    checkPermissionErrorResponseBody(response, 400);
  }

  @Test
  public void shouldHaveErrorIfValidationFails() throws IOException {
    UUID userId = authenticationHelper.getCurrentUser().getId();

    requisitions.forEach(requisition ->
        doReturn(ValidationResult.success())
            .when(requisitionService)
            .validateCanApproveRequisition(refEq(requisition), eq(userId))
    );

    requisitions = requisitions.stream()
        .map(this::spyRequisitionAndStubRepository)
        .collect(Collectors.toList());
    doReturn(ValidationResult.fieldErrors(Maps.newHashMap("someField", new Message("some-key"))))
        .when(requisitions.get(0)).validateCanChangeStatus(any(LocalDate.class), anyBoolean());

    Response response = post(APPROVE_ALL, requisitionIds);
    checkValidationErrorResponseBody(response, 400,
        requisitionErrors -> requisitionErrors.get(0)
            .get("fieldErrors")
            .get("someField")
            .get("messageKey")
            .asText(),
        "some-key");
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

    doReturn(ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_APPROVE))
        .when(requisitionService).validateCanSaveRequisition(requisitions.get(0).getId());

    Response response = put(SAVE_ALL, approveRequisitions);
    checkPermissionErrorResponseBody(response, 400);
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

  private void mockRepositorySaveAnswer() {
    given(requisitionRepository.save(any(Requisition.class))).willAnswer(new SaveAnswer<>());
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
    given(requisitionDtoBuilder
        .buildBatch(any(Requisition.class), any(FacilityDto.class),
            anyMapOf(UUID.class, OrderableDto.class), any(ProcessingPeriodDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.build(anyListOf(Requisition.class)))
        .willAnswer(new BuildListOfRequisitionDtosAnswer());
  }

  private void mockStockEventServiceResponses() {
    when(stockEventBuilder.fromRequisition(any())).thenReturn(new StockEventDto());
    doNothing().when(stockEventStockManagementService).submit(any(StockEventDto.class));
  }

  protected static class BuildRequisitionDtoAnswer implements Answer<RequisitionDto> {

    @Override
    public RequisitionDto answer(InvocationOnMock invocation) {
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
      dto.setRequisitionLineItems(requisition
          .getRequisitionLineItems()
          .stream()
          .map(line -> {
            OrderableDto orderableDto = new OrderableDto();
            orderableDto.setId(line.getOrderableId());
            orderableDto.setPrograms(Sets.newHashSet());
            orderableDto.setProductCode(RandomStringUtils.randomAlphanumeric(5));
            orderableDto.setFullProductName(RandomStringUtils.randomAlphanumeric(5));

            RequisitionLineItemDto lineDto = new RequisitionLineItemDto();
            line.export(lineDto, orderableDto);

            return lineDto;
          })
          .collect(Collectors.toList()));

      FacilityDto facility = null;
      if (requisition.getFacilityId() != null) {
        facility = new FacilityDto();
        facility.setId(requisition.getFacilityId());
        facility.setName("facility");
      }

      ProgramDto program = null;
      if (requisition.getProgramId() != null) {
        program = new ProgramDto();
        program.setId(requisition.getProgramId());
        program.setName("program");
      }

      ProcessingPeriodDto period = null;
      if (requisition.getProcessingPeriodId() != null) {
        period = new ProcessingPeriodDto();
        period.setId(requisition.getProcessingPeriodId());
        period.setName("period");
      }

      dto.setProcessingPeriod(period);
      dto.setFacility(facility);
      dto.setProgram(program);

      return dto;
    }
  }

  protected static class BuildListOfRequisitionDtosAnswer implements Answer<List<RequisitionDto>> {

    @Override
    public List<RequisitionDto> answer(InvocationOnMock invocation) {
      Collection<Requisition> collection = (Collection) invocation.getArguments()[0];

      if (null == collection) {
        return Collections.emptyList();
      }

      return collection
          .stream()
          .map(BatchRequisitionControllerIntegrationTest.BuildRequisitionDtoAnswer::export)
          .collect(Collectors.toList());
    }
  }

}
