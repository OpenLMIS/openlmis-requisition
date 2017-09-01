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

package org.openlmis.utils;

import static org.assertj.core.api.Java6Assertions.assertThat;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.RequisitionLineItem.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_RECEIVED_QUANTITY;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventAdjustmentDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventLineItemDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockCardStockManagementService;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@RunWith(PowerMockRunner.class)
@PrepareForTest(StockEventBuilder.class)
@SuppressWarnings("PMD.TooManyMethods")
public class StockEventBuilderTest {

  private static final LocalDate DATE_PHYSICAL_STOCK_COUNT_COMPLETED = LocalDate.now().minusDays(3);
  private static final LocalDate PERIOD_END_DATE = LocalDate.now().minusDays(1);
  private static final String RECEIPTS_REASON_ID = "RECEIPTS_REASON_ID";
  private static final String CONSUMED_REASON_ID = "CONSUMED_REASON_ID";
  private static final String BEGINNING_BALANCE_EXCESS_REASON_ID =
      "BEGINNING_BALANCE_EXCESS_REASON_ID";
  private static final String BEGINNING_BALANCE_INSUFFICIENCY_REASON_ID =
      "BEGINNING_BALANCE_INSUFFICIENCY_REASON_ID";
  private static final ZoneId ZONE_ID = ZoneId.systemDefault();

  private Requisition requisition;

  private RequisitionTemplateColumn totalReceivedQuantityColumn;

  private RequisitionTemplateColumn totalConsumedQuantityColumn;

  private RequisitionTemplateColumn totalLossesAndAdjustmentsColumn;

  private RequisitionTemplateColumn stockOnHandColumn;

  private RequisitionLineItem lineItemOneDto;

  private RequisitionLineItem lineItemTwoDto;

  private List<StockAdjustmentReason> reasons;

  private StockAdjustmentReason receiptsReason;

  private StockAdjustmentReason consumedReason;

  private StockAdjustmentReason beginningBalanceExcess;

  private StockAdjustmentReason beginningBalanceInsufficiency;

  private ProcessingPeriodDto period;

  private UUID userId = UUID.randomUUID();

  private List<StockCardDto> stockCards;

  @Mock
  private DateHelper dateHelper;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private StockCardStockManagementService stockCardStockManagementService;

  @InjectMocks
  private StockEventBuilder stockEventBuilder;

  @Before
  public void setUp() {
    preparePeriod();
    prepareRequisitionDto();

    PowerMockito.mockStatic(System.class);
    PowerMockito.when(System.getenv(RECEIPTS_REASON_ID))
        .thenReturn(receiptsReason.getReasonId().toString());
    PowerMockito.when(System.getenv(CONSUMED_REASON_ID))
        .thenReturn(consumedReason.getReasonId().toString());
    PowerMockito.when(System.getenv(BEGINNING_BALANCE_EXCESS_REASON_ID))
        .thenReturn(beginningBalanceExcess.getReasonId().toString());
    PowerMockito.when(System.getenv(BEGINNING_BALANCE_INSUFFICIENCY_REASON_ID))
        .thenReturn(beginningBalanceInsufficiency.getReasonId().toString());

    when(dateHelper.getZone()).thenReturn(ZONE_ID);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);
    UserDto user = new UserDto();
    user.setId(userId);
    when(authenticationHelper.getCurrentUser()).thenReturn(user);
    when(stockCardStockManagementService.getStockCards(
        requisition.getFacilityId(),
        requisition.getProgramId())
    ).thenReturn(stockCards);
  }

  @Test
  public void itShouldIncludeReceiptsIfTotalReceivedQuantityIsDisplayed() throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, receiptsReason)).isTrue());
  }

  @Test
  public void itShouldNotIncludeReceiptsIfTotalReceivedQuantityDoesNotExist() throws Exception {
    requisition.getTemplate().getColumnsMap().remove(TOTAL_RECEIVED_QUANTITY);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, receiptsReason)).isFalse());
  }

  @Test
  public void itShouldNotIncludeReceiptsIfTotalReceivedQuantityIsNotDisplayed() throws Exception {
    totalReceivedQuantityColumn.setIsDisplayed(false);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, receiptsReason)).isFalse());
  }

  @Test
  public void itShouldNotIncludeReceiptsIfTheReasonDoesNotExist() throws Exception {
    reasons.remove(receiptsReason);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, receiptsReason)).isFalse());
  }

  @Test
  public void itShouldNotIncludeReceiptsIfTheReasonIdIsNotGivenInEnv() throws Exception {
    PowerMockito.when(System.getenv(RECEIPTS_REASON_ID)).thenReturn(null);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, receiptsReason)).isFalse());
  }

  @Test
  public void itShouldIncludeConsumedIfTotalConsumedQuantityIsDisplayed() throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, consumedReason)).isTrue());
  }

  @Test
  public void itShouldIncludeConsumedIfTotalConsumedQuantityIsNotDisplayed() throws Exception {
    totalConsumedQuantityColumn.setIsDisplayed(false);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, consumedReason)).isTrue());
  }

  @Test
  public void itShouldNotIncludeConsumedIfTotalConsumedQuantityDoesNotExist() throws Exception {
    requisition.getTemplate().getColumnsMap().remove(TOTAL_CONSUMED_QUANTITY);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, consumedReason)).isFalse());
  }

  @Test
  public void itShouldNotIncludeConsumedIfTheReasonDoesNotExist() throws Exception {
    reasons.remove(consumedReason);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, consumedReason)).isFalse());
  }

  @Test
  public void itShouldNotIncludeConsumedIfTheReasonIdIsNotGivenInEnv() throws Exception {
    PowerMockito.when(System.getenv(CONSUMED_REASON_ID)).thenReturn(null);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isGreaterThan(0);
    result.getLineItems()
        .forEach(lineItem -> assertThat(containsReason(lineItem, consumedReason)).isFalse());
  }

  @Test
  public void itShouldNotIncludeAdditionalAdjustmentsIfTotalLossesAndAdjustmentsAreNotDisplayed()
      throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().get(0).getStockAdjustments().size()).isEqualTo(4);
    assertThat(result.getLineItems().get(0).getStockAdjustments().get(0))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(0)), 24));
    assertThat(result.getLineItems().get(0).getStockAdjustments().get(1))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(2)), 25));

    assertThat(result.getLineItems().get(1).getStockAdjustments().size()).isEqualTo(4);
    assertThat(result.getLineItems().get(1).getStockAdjustments().get(0))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(1)), 37));
    assertThat(result.getLineItems().get(1).getStockAdjustments().get(1))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(3)), 38));
  }

  @Test
  public void itShouldIncludeAdditionalAdjustmentsIfTotalLossesAndAdjustmentsAreDisplayed()
      throws Exception {
    totalLossesAndAdjustmentsColumn.setIsDisplayed(false);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().get(0).getStockAdjustments().size()).isEqualTo(2);
    assertThat(result.getLineItems().get(0).getStockAdjustments().get(0))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(4)), 23));
    assertThat(result.getLineItems().get(0).getStockAdjustments().get(1))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(5)), 22));

    assertThat(result.getLineItems().get(1).getStockAdjustments().size()).isEqualTo(2);
    assertThat(result.getLineItems().get(1).getStockAdjustments().get(0))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(4)), 36));
    assertThat(result.getLineItems().get(1).getStockAdjustments().get(1))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(reasons.get(5)), 35));
  }

  @Test
  public void itShouldMapStockOnHandIfItIsDisplayed() throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(2);
    assertThat(result.getLineItems().get(0).getQuantity())
        .isEqualTo(lineItemOneDto.getStockOnHand());
    assertThat(result.getLineItems().get(1).getQuantity())
        .isEqualTo(lineItemTwoDto.getStockOnHand());
  }

  @Test
  public void itShouldMapStockOnHandEvenIfItIsNotDisplayed() throws Exception {
    stockOnHandColumn.setIsDisplayed(false);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(2);
    assertThat(result.getLineItems().get(0).getQuantity())
        .isEqualTo(lineItemOneDto.getStockOnHand());
    assertThat(result.getLineItems().get(1).getQuantity())
        .isEqualTo(lineItemTwoDto.getStockOnHand());
  }

  @Test
  public void itShouldUseDatePhysicalStockCountCompletedIfItIsGiven() throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(2);
    result.getLineItems().forEach(lineItem -> assertThat(lineItem.getOccurredDate())
        .isEqualByComparingTo(DATE_PHYSICAL_STOCK_COUNT_COMPLETED));
  }

  @Test
  public void itShouldUseSubmittedDateOnEmergencyRequisition() throws Exception {
    requisition.setDatePhysicalStockCountCompleted(null);
    requisition.setEmergency(true);

    StatusChange statusChange = new StatusChange();
    ZonedDateTime dateTime = ZonedDateTime.now();
    statusChange.setStatus(RequisitionStatus.SUBMITTED);
    statusChange.setCreatedDate(dateTime);
    requisition.setStatusChanges(Arrays.asList(statusChange));

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(2);
    result.getLineItems().forEach(lineItem -> assertThat(lineItem.getOccurredDate())
        .isEqualByComparingTo(dateTime.toLocalDate()));
  }

  @Test
  public void itShouldUsePeriodEndDateIfDatePhysicalStockCountCompletedIsNull() throws Exception {
    requisition.setDatePhysicalStockCountCompleted(null);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(2);
    result.getLineItems().forEach(lineItem -> assertThat(lineItem.getOccurredDate())
        .isEqualByComparingTo(PERIOD_END_DATE));

  }

  @Test
  public void itShouldIncludeOrderableIds() throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().get(0).getOrderableId())
        .isEqualTo(lineItemOneDto.getOrderableId());
    assertThat(result.getLineItems().get(1).getOrderableId())
        .isEqualTo(lineItemTwoDto.getOrderableId());
  }

  @Test
  public void itShouldIgnoreSkippedLineItems() throws Exception {
    lineItemOneDto.setSkipped(true);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(1);
    assertThat(result.getLineItems().get(0).getQuantity())
        .isNotEqualTo(lineItemOneDto.getStockOnHand());
  }

  @Test
  public void itShouldIgnoreNonFullSupplyLineItems() throws Exception {
    lineItemOneDto.setNonFullSupply(true);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().size()).isEqualTo(1);
    assertThat(result.getLineItems().get(0).getQuantity())
        .isNotEqualTo(lineItemOneDto.getStockOnHand());
  }

  @Test
  public void itShouldIncludeUserId() throws Exception {
    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getUserId())
        .isEqualTo(userId);
  }

  @Test
  public void itShouldIncludeBeginningBalanceExcessIfBeginningBalanceIsBiggerThanStockOnHand() {
    lineItemOneDto.setBeginningBalance(20);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().get(0).getStockAdjustments()).isEqualTo(5);
    assertThat(result.getLineItems().get(0).getStockAdjustments().get(4))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(beginningBalanceInsufficiency), 10));
  }

  @Test
  public void
      itShouldIncludeBeginningBalanceInsufficiencyIfBeginningBalanceIsLowerThanStockOnHand() {
    lineItemTwoDto.setBeginningBalance(33);

    StockEventDto result = stockEventBuilder.fromRequisition(requisition);

    assertThat(result.getLineItems().get(1).getStockAdjustments()).isEqualTo(5);
    assertThat(result.getLineItems().get(1).getStockAdjustments().get(4))
        .isEqualToComparingFieldByFieldRecursively(new StockEventAdjustmentDto(
            ReasonDto.newInstance(beginningBalanceExcess), 3));
  }

  private RequisitionLineItem prepareLineItemOneDto() {
    lineItemOneDto = new RequisitionLineItem();

    lineItemOneDto.setSkipped(false);
    lineItemOneDto.setBeginningBalance(30);
    lineItemOneDto.setStockOnHand(21);
    lineItemOneDto.setTotalReceivedQuantity(22);
    lineItemOneDto.setTotalConsumedQuantity(23);
    lineItemOneDto.setStockAdjustments(Arrays.asList(
        prepareStockAdjustment(reasons.get(0), 24),
        prepareStockAdjustment(reasons.get(2), 25)
    ));
    lineItemOneDto.setOrderableId(UUID.randomUUID());
    stockCards.add(StockCardDto.builder()
        .orderable(OrderableDto.builder().id(lineItemOneDto.getOrderableId()).build())
        .stockOnHand(30)
        .build());

    return lineItemOneDto;
  }

  private RequisitionLineItem prepareLineItemTwoDto() {
    lineItemTwoDto = new RequisitionLineItem();

    lineItemTwoDto.setSkipped(false);
    lineItemTwoDto.setBeginningBalance(30);
    lineItemTwoDto.setStockOnHand(34);
    lineItemTwoDto.setTotalReceivedQuantity(35);
    lineItemTwoDto.setTotalConsumedQuantity(36);
    lineItemTwoDto.setStockAdjustments(Arrays.asList(
        prepareStockAdjustment(reasons.get(1), 37),
        prepareStockAdjustment(reasons.get(3), 38)
    ));
    lineItemTwoDto.setOrderableId(UUID.randomUUID());
    stockCards.add(StockCardDto.builder()
        .orderable(OrderableDto.builder().id(lineItemTwoDto.getOrderableId()).build())
        .stockOnHand(30)
        .build());

    return lineItemTwoDto;
  }

  private void prepareRequisitionDto() {
    requisition = new Requisition();
    stockCards = new ArrayList<>();

    requisition.setDatePhysicalStockCountCompleted(DATE_PHYSICAL_STOCK_COUNT_COMPLETED);
    requisition.setTemplate(prepareTemplate());
    requisition.setFacilityId(UUID.randomUUID());
    requisition.setProgramId(UUID.randomUUID());
    requisition.setStockAdjustmentReasons(prepareReasons());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setRequisitionLineItems(Arrays.asList(
        prepareLineItemOneDto(),
        prepareLineItemTwoDto()
    ));
    requisition.setEmergency(false);
  }

  private RequisitionTemplate prepareTemplate() {
    RequisitionTemplate template = new RequisitionTemplate();

    template.setColumnsMap(prepareColumnsMap());

    return template;
  }

  private Map<String, RequisitionTemplateColumn> prepareColumnsMap() {
    totalReceivedQuantityColumn = prepareColumn(TOTAL_RECEIVED_QUANTITY);
    totalConsumedQuantityColumn = prepareColumn(TOTAL_CONSUMED_QUANTITY);
    totalLossesAndAdjustmentsColumn = prepareColumn(TOTAL_LOSSES_AND_ADJUSTMENTS);
    stockOnHandColumn = prepareColumn(STOCK_ON_HAND);

    Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<>();
    columnsMap.put(TOTAL_CONSUMED_QUANTITY, totalConsumedQuantityColumn);
    columnsMap.put(TOTAL_RECEIVED_QUANTITY, totalReceivedQuantityColumn);
    columnsMap.put(TOTAL_LOSSES_AND_ADJUSTMENTS, totalLossesAndAdjustmentsColumn);
    columnsMap.put(STOCK_ON_HAND, stockOnHandColumn);

    return columnsMap;
  }

  private RequisitionTemplateColumn prepareColumn(String columnName) {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();

    column.setName(columnName);
    column.setIsDisplayed(true);

    return column;
  }

  private List<StockAdjustmentReason> prepareReasons() {
    consumedReason = prepareReason("Consumed");
    receiptsReason = prepareReason("Receipts");
    beginningBalanceExcess = prepareReason("Beginning Balance Excess");
    beginningBalanceInsufficiency = prepareReason("Beginning Balance Insufficiency");

    reasons = new ArrayList<>();

    reasons.add(prepareReason("Reason One"));
    reasons.add(prepareReason("Reason Two"));
    reasons.add(prepareReason("Reason Three"));
    reasons.add(prepareReason("Reason Four"));
    reasons.add(consumedReason);
    reasons.add(receiptsReason);
    reasons.add(beginningBalanceExcess);
    reasons.add(beginningBalanceInsufficiency);

    return reasons;
  }

  private boolean containsReason(StockEventLineItemDto lineItem,
                                 StockAdjustmentReason reason) {
    return lineItem.getStockAdjustments().stream()
        .filter(stockAdjustment -> stockAdjustment.getReason().equals(
            ReasonDto.newInstance(reason)))
        .collect(Collectors.toList()).size() > 0;
  }

  private void preparePeriod() {
    period = new ProcessingPeriodDto();
    period.setEndDate(PERIOD_END_DATE);
  }

  private StockAdjustmentReason prepareReason(String reasonName) {
    StockAdjustmentReason reason = new StockAdjustmentReason();

    reason.setReasonId(UUID.randomUUID());
    reason.setName(reasonName);

    return reason;
  }

  private StockAdjustment prepareStockAdjustment(StockAdjustmentReason reason, Integer quantity) {
    StockAdjustment stockAdjustment = new StockAdjustment();

    stockAdjustment.setQuantity(quantity);
    stockAdjustment.setReasonId(reason.getReasonId());

    return stockAdjustment;
  }

}
