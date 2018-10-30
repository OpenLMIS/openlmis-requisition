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

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.PRICE_PER_PACK_IF_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.web.utils.WireMockResponses.MOCK_CHECK_RESULT;
import static org.openlmis.requisition.web.utils.WireMockResponses.MOCK_TOKEN_REQUEST_RESPONSE;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.jayway.restassured.RestAssured;
import com.jayway.restassured.config.ObjectMapperConfig;
import com.jayway.restassured.config.RestAssuredConfig;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;
import org.assertj.core.util.Lists;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Rule;
import org.junit.runner.RunWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.BaseTimestampedEntity;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.validate.RequisitionValidationTestUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.embedded.LocalServerPort;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;


@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = RANDOM_PORT)
@ActiveProfiles("test")
@DirtiesContext
@SuppressWarnings("PMD.TooManyMethods")
public abstract class BaseWebIntegrationTest {
  static final String BASE_URL = System.getenv("BASE_URL");

  static final String PERMISSION_ERROR_MESSAGE = ERROR_NO_FOLLOWING_PERMISSION;
  static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML definition.";
  static final UUID LINE_ITEM_PRODUCT_ID = UUID.randomUUID();

  static final String SORT = "sort";
  static final String PAGE = "page";

  static final String FILTER_VALUE = "filterValue";
  static final String FILTER_BY = "filterBy";

  static final Pageable FIRST_PAGE =
      new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION);

  @Rule
  public WireMockRule wireMockRule = new WireMockRule(80);

  @MockBean
  protected AuthenticationHelper authenticationHelper;

  @MockBean
  protected PermissionService permissionService;

  @MockBean
  protected RequisitionRepository requisitionRepository;

  @MockBean
  protected PeriodReferenceDataService periodReferenceDataService;

  @MockBean(name = "programReferenceDataService")
  protected ProgramReferenceDataService programReferenceDataService;

  protected RestAssuredClient restAssured;

  @MockBean
  protected SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @MockBean
  protected SupplyLineReferenceDataService supplyLineReferenceDataService;

  @MockBean
  protected RequisitionTemplateService requisitionTemplateService;

  @Autowired
  protected ObjectMapper objectMapper;

  @LocalServerPort
  private int serverPort;

  @Value("${currencyCode}")
  private String currencyCode;

  /**
   * Method called to initialize basic resources after the object is created.
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

  protected UserDto mockUserAuthenticated() {
    UserDto user = new UserDto();
    user.setId(UUID.randomUUID());
    user.setFirstName("admin");
    user.setLastName("strator");
    user.setEmail("admin@openlmis.org");

    given(authenticationHelper.getCurrentUser()).willReturn(user);

    return user;
  }

  protected UUID anyUuid() {
    return any(UUID.class);
  }

  protected String getTokenHeader() {
    return "Bearer " + UUID.randomUUID().toString();
  }

  protected final Requisition generateRequisition() {
    return generateRequisition(RequisitionStatus.INITIATED);
  }

  protected final Requisition generateRequisition(RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition(
        UUID.randomUUID(), generateProgram().getId(),
        generateProcessingPeriod(), requisitionStatus, true
    );

    requisition.setId(UUID.randomUUID());
    requisition.setVersion(1L);
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setRequisitionLineItems(generateRequisitionLineItems(requisition));
    requisition.setTemplate(generateRequisitionTemplate());
    requisition.setEmergency(false);
    requisition.setAvailableProducts(Collections.emptySet());

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);
    return requisition;
  }

  protected final Requisition generateRequisition(RequisitionStatus requisitionStatus,
                                                  UUID programId, UUID facilityId) {
    Requisition requisition = new Requisition(
        facilityId, programId, UUID.randomUUID(), requisitionStatus, true);

    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setRequisitionLineItems(new ArrayList<>());
    requisition.setTemplate(generateRequisitionTemplate());
    requisition.setProcessingPeriodId(generateProcessingPeriod());

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisition);
    return requisition;
  }

  protected final RequisitionTemplate generateRequisitionTemplate() {
    RequisitionTemplate template = new RequisitionTemplateDataBuilder()
        .withColumns(RequisitionValidationTestUtils.initiateColumns())
        .build();

    given(requisitionTemplateService.findTemplate(anyUuid(), anyUuid(), eq(false)))
        .willReturn(template);

    return template;
  }

  protected OrderableDto generateOrderable(UUID id, List<Requisition> requisitions) {
    Set<ProgramOrderableDto> programOrderables = requisitions.stream()
        .map(r -> getProgramOrderableDto(r.getProgramId()))
        .collect(Collectors.toSet());

    OrderableDto orderable = new OrderableDto();
    orderable.setId(id);
    orderable.setPrograms(programOrderables);

    return orderable;
  }

  private static ProgramOrderableDto getProgramOrderableDto(UUID programId) {
    ProgramOrderableDto programOrderableDto = new ProgramOrderableDto();
    programOrderableDto.setProgramId(programId);
    programOrderableDto.setPricePerPack(Money.of(CurrencyUnit.EUR, 10));
    return programOrderableDto;
  }

  private List<RequisitionLineItem> generateRequisitionLineItems(Requisition requisition) {
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setOrderableId(LINE_ITEM_PRODUCT_ID);
    lineItem.setRequisition(requisition);
    lineItem.setId(UUID.randomUUID());
    lineItem.setPricePerPack(Money.of(CurrencyUnit.of(currencyCode), PRICE_PER_PACK_IF_NULL));
    lineItem.setTotalCost(Money.of(CurrencyUnit.of(currencyCode), PRICE_PER_PACK_IF_NULL));

    return Lists.newArrayList(lineItem);
  }

  private UUID generateProcessingPeriod() {
    ProcessingPeriodDto period = new ProcessingPeriodDto();

    period.setId(UUID.randomUUID());
    period.setEndDate(LocalDate.now().minusDays(5));

    when(periodReferenceDataService.findOne(eq(period.getId()))).thenReturn(period);

    return period.getId();
  }

  ProgramDto generateProgram() {
    ProgramDto program = new ProgramDtoDataBuilder()
        .build();

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);

    return program;
  }

  protected void mockExternalAuthorization() {
    // This mocks the auth check to always return valid admin credentials.
    wireMockRule.stubFor(post(urlEqualTo("/api/oauth/check_token"))
        .willReturn(aResponse()
            .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .withBody(MOCK_CHECK_RESULT)));

    // This mocks the auth token request response
    wireMockRule.stubFor(post(urlPathEqualTo("/api/oauth/token?grant_type=client_credentials"))
        .willReturn(aResponse()
            .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .withBody(MOCK_TOKEN_REQUEST_RESPONSE)));

  }

  protected Requisition spyRequisitionAndStubRepository(RequisitionStatus status) {
    return spyRequisitionAndStubRepository(generateRequisition(status));
  }

  protected Requisition spyRequisitionAndStubRepository(Requisition requisition) {
    Requisition requisitionSpy = spy(requisition);

    given(requisitionRepository.findOne(requisition.getId())).willReturn(requisitionSpy);
    doReturn(ValidationResult.success())
        .when(requisitionSpy).validateCanChangeStatus(any(LocalDate.class), anyBoolean());
    return requisitionSpy;
  }

  void mockSearchSupervisoryNodeByProgramAndFacility() {
    SupervisoryNodeDto supervisoryNode = new SupervisoryNodeDto();
    supervisoryNode.setId(UUID.randomUUID());
    given(supervisoryNodeReferenceDataService.findSupervisoryNode(anyUuid(), anyUuid()))
        .willReturn(supervisoryNode);
  }

  static class SaveAnswer<T extends BaseEntity> implements Answer<T> {

    @Override
    public T answer(InvocationOnMock invocation) throws Throwable {
      T obj = (T) invocation.getArguments()[0];

      if (null == obj) {
        return null;
      }

      if (null == obj.getId()) {
        obj.setId(UUID.randomUUID());

        if (obj instanceof BaseTimestampedEntity) {
          ((BaseTimestampedEntity) obj).setCreatedDate(ZonedDateTime.now());
        }
      }

      extraSteps(obj);

      return obj;
    }

    void extraSteps(T obj) {
      // should be overridden if any extra steps are required.
    }

  }
}
