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

package org.openlmis.requisition.service;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.anyList;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.SourceType.USER_INPUT;
import static org.openlmis.requisition.web.utils.WireMockResponses.MOCK_CHECK_RESULT;
import static org.openlmis.requisition.web.utils.WireMockResponses.MOCK_TOKEN_REQUEST_RESPONSE;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.jayway.restassured.RestAssured;
import com.jayway.restassured.config.ObjectMapperConfig;
import com.jayway.restassured.config.RestAssuredConfig;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.annotation.PostConstruct;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ReleasableRequisitionBatchDto;
import org.openlmis.requisition.dto.ReleasableRequisitionDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.ReleasableRequisitionBatchDtoDataBuilder;
import org.openlmis.requisition.testutils.ReleasableRequisitionDtoDataBuilder;
import org.openlmis.requisition.testutils.UserDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = RANDOM_PORT)
@ActiveProfiles("test")
@DirtiesContext()
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionServiceIntegrationTest {

  private static final String BASE_URL = System.getenv("BASE_URL");
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String BATCH_RELEASES_URL = RESOURCE_URL + "/batchReleases";
  private static final UUID APPROVED_QTY_UUID = UUID.fromString(
      "a62a5fed-c0b6-4d49-8a96-c631da0d0113");

  @Rule
  public WireMockRule wireMockRule = new WireMockRule(80);

  @LocalServerPort
  private int serverPort;

  @Captor
  private ArgumentCaptor<List<OrderDto>> ordersCaptor;

  @MockBean
  private PermissionService permissionService;

  @MockBean
  private AuthenticationHelper authenticationHelper;

  @MockBean(name = "facilityReferenceDataService")
  private FacilityReferenceDataService facilityReferenceDataService;

  @MockBean
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  @MockBean
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @MockBean
  private OrderFulfillmentService orderFulfillmentService;

  @Autowired
  private AvailableRequisitionColumnRepository availableColumnRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @MockBean
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private ObjectMapper objectMapper;

  private RestAssuredClient restAssured;

  private List<SupplyLineDto> managedSupplyLines;

  private final UUID supervisoryNodeId = UUID.randomUUID();

  private RequisitionTemplate template;

  private FacilityDto facility;

  /**
   * Method called to initialize basic resources after the object is created.
   * We use rest assured because transactions of that service starts at controller layer.
   */
  @PostConstruct
  public void init() {
    mockExternalAuthorization();

    RestAssured.baseURI = BASE_URL;
    RestAssured.port = serverPort;
    RestAssured.config = RestAssuredConfig.config().objectMapperConfig(
        new ObjectMapperConfig().jackson2ObjectMapperFactory((clazz, charset) -> objectMapper)
    );
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    RamlDefinition ramlDefinition = RamlLoaders.fromClasspath()
        .load("api-definition-raml.yaml").ignoringXheaders();
    restAssured = ramlDefinition.createRestAssured();
  }

  @Before
  public void setUp() {
    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());
    mockUserAuthenticated();

    facility = mockFacility();
    RequisitionTemplateColumn column = getApprovedQtyRequisitionTemplateColumn();
    template = getRequisitionTemplate(column);

    SupplyLineDto supplyLineDto = new SupplyLineDto();
    supplyLineDto.setSupplyingFacility(facility);
    managedSupplyLines = Collections.singletonList(supplyLineDto);

    RightDto right = mockViewOrdersRight();
    Set<FacilityDto> managedFacilities = Collections.singleton(facility);
    given(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(
        any(UUID.class), eq(right.getId()))).willReturn(managedFacilities);

    doNothing().when(orderFulfillmentService).create(ordersCaptor.capture());
  }

  @Test
  public void shouldSuccessfullySaveAndBuildOrder() {
    final long reqsCountBefore = requisitionRepository.count();
    Requisition requisition = mockAndSaveRequisition();
    ReleasableRequisitionBatchDto releaseDto = generateReleaseRequisitionDto(
        Collections.singletonList(
            getReleasableRequisitionDto(requisition)
        ));

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(releaseDto)
        .when()
        .post(BATCH_RELEASES_URL)
        .then()
        .statusCode(HttpStatus.CREATED.value());

    final long reqsCountAfter = requisitionRepository.count();
    List<OrderDto> orders = ordersCaptor.getValue();
    assertThat(orders, hasSize(1));
    assertEquals(orders.get(0).getExternalId(), requisition.getId());
    assertEquals(reqsCountBefore + 1, reqsCountAfter);
  }

  @Test
  public void shouldSuccessfullySaveAndBuildOrders() {
    final long reqsCountBefore = requisitionRepository.count();
    Requisition requisition = mockAndSaveRequisition();
    Requisition requisition2 = mockAndSaveRequisition();
    ReleasableRequisitionBatchDto releaseDto = generateReleaseRequisitionDto(Arrays.asList(
        getReleasableRequisitionDto(requisition),
        getReleasableRequisitionDto(requisition2)
    ));

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(releaseDto)
        .when()
        .post(BATCH_RELEASES_URL)
        .then()
        .statusCode(HttpStatus.CREATED.value());

    final long reqsCountAfter = requisitionRepository.count();
    List<OrderDto> orders = ordersCaptor.getValue();
    assertThat(orders, hasSize(2));
    assertEquals(orders.get(0).getExternalId(), requisition.getId());
    assertEquals(orders.get(1).getExternalId(), requisition2.getId());
    assertEquals(reqsCountBefore + 2, reqsCountAfter);
  }

  @Test
  public void shouldNotBuildOrdersIfSaveFailed() {
    Requisition requisition = mockAndSaveRequisition();
    Requisition requisition2 = mockAndSaveRequisition();
    ReleasableRequisitionBatchDto releaseDto = generateReleaseRequisitionDto(Arrays.asList(
        getReleasableRequisitionDto(requisition),
        getReleasableRequisitionDto(requisition2)
    ));

    mockFailDuringBuildingOrders();

    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(releaseDto)
        .when()
        .post(BATCH_RELEASES_URL)
        .then()
        .statusCode(HttpStatus.BAD_REQUEST.value());

    verify(orderFulfillmentService, times(0)).create(anyList());
  }

  /**
   * https://openlmis.atlassian.net/browse/OLMIS-6876 indicates that there is some exception
   * after saving the requisitions and before the return statement of
   * RequisitionService.convertToOrder. We don't expect requisitionStatusProcessor
   * to throw any error during the runtime, but it is helpful to mock
   * DataIntegrityViolationException thrown after sending orders to fulfillment service.
   */
  private void mockFailDuringBuildingOrders() {
    doThrow(new DataIntegrityViolationException("test"))
        .when(requisitionStatusProcessor).statusChange(any(), any());
  }

  private ReleasableRequisitionDto getReleasableRequisitionDto(Requisition requisition) {
    return new ReleasableRequisitionDtoDataBuilder()
        .withRequisitionId(requisition.getId())
        .withSupplyingDepotId(facility.getId())
        .buildAsDto();
  }

  private Requisition mockAndSaveRequisition() {
    return mockAndSaveRequisition(null);
  }

  private Requisition mockAndSaveRequisition(Requisition req) {
    Requisition requisition = saveRequisition(req);
    given(supplyLineReferenceDataService.search(
        eq(requisition.getProgramId()), eq(supervisoryNodeId))).willReturn(managedSupplyLines);
    return requisition;
  }

  private Requisition saveRequisition(Requisition requisition) {
    if (requisition == null) {
      return requisitionRepository.save(generateRequisition());
    } else {
      return requisitionRepository.save(requisition);
    }
  }

  private Requisition generateRequisition() {
    Requisition requisition = RequisitionBuilder.newRequisition(UUID.randomUUID(),
        UUID.randomUUID(), false);
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setProcessingPeriodId(UUID.randomUUID());
    requisition.setStatus(RequisitionStatus.APPROVED);
    requisition.setSupervisoryNodeId(supervisoryNodeId);

    return requisition;
  }

  private RequisitionTemplateColumn getApprovedQtyRequisitionTemplateColumn() {
    AvailableRequisitionColumn column = availableColumnRepository.findById(APPROVED_QTY_UUID).get();
    column.setOptions(null);

    return new RequisitionTemplateColumnDataBuilder()
        .withColumnDefinition(column)
        .withName(column.getName())
        .withLabel(column.getLabel())
        .withIndicator(column.getIndicator())
        .withDefinition(column.getDefinition())
        .withDisplayOrder(1)
        .withDisplay(true)
        .withSource(USER_INPUT)
        .withOption(null)
        .withTag(null)
        .build();
  }

  private RequisitionTemplate getRequisitionTemplate(RequisitionTemplateColumn column) {
    RequisitionTemplate template = new RequisitionTemplate(UUID.randomUUID(),
        6, false, UUID.randomUUID().toString(),
        Collections.singletonMap(column.getName(), column),
        new HashSet<>(), false);
    template = requisitionTemplateRepository.save(template);
    return template;
  }

  private ReleasableRequisitionBatchDto generateReleaseRequisitionDto(
      List<ReleasableRequisitionDto> requisitions) {
    ReleasableRequisitionBatchDto batch = new ReleasableRequisitionBatchDtoDataBuilder()
        .withRequisitionsToRelease(requisitions)
        .buildAsDto();
    batch.setCreateOrder(true);
    return batch;
  }

  private String getTokenHeader() {
    return "Bearer " + UUID.randomUUID().toString();
  }

  private void mockExternalAuthorization() {
    // This mocks the auth check to always return valid admin credentials.
    wireMockRule.stubFor(WireMock.post(urlEqualTo("/api/oauth/check_token"))
        .willReturn(aResponse()
            .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .withBody(MOCK_CHECK_RESULT)));

    // This mocks the auth token request response
    wireMockRule.stubFor(WireMock.post(
        urlPathEqualTo("/api/oauth/token?grant_type=client_credentials"))
        .willReturn(aResponse()
            .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .withBody(MOCK_TOKEN_REQUEST_RESPONSE)));
  }

  private UserDto mockUserAuthenticated() {
    UserDto user = new UserDtoDataBuilder().buildAsDto();
    given(authenticationHelper.getCurrentUser()).willReturn(user);
    return user;
  }

  private RightDto mockViewOrdersRight() {
    RightDto right = new RightDto();
    right.setId(UUID.randomUUID());
    given(authenticationHelper.getRight(PermissionService.ORDERS_EDIT)).willReturn(right);
    return right;
  }

  FacilityDto mockFacility() {
    FacilityDto facilityDto = DtoGenerator.of(FacilityDto.class);
    facilityDto.getType().setId(UUID.randomUUID());

    when(facilityReferenceDataService.findOne(facilityDto.getId())).thenReturn(facilityDto);

    return facilityDto;
  }

}
