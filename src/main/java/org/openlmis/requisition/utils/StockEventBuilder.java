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

package org.openlmis.requisition.utils;

import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.stockmanagement.StockCardDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventAdjustmentDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventLineItemDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockCardStockManagementService;
import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StockEventBuilder {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(StockEventBuilder.class);

  private static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  private static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";

  private static final Logger LOGGER = LoggerFactory.getLogger(StockEventBuilder.class);

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private StockCardStockManagementService stockCardService;

  @Autowired
  private ConfigurationSettingService settings;

  /**
   * Builds a physical inventory draft DTO from the given requisition.
   *
   * @param requisition  the requisition to be used a source for the physical inventory draft
   * @return  the create physical inventory draft
   */
  public StockEventDto fromRequisition(Requisition requisition) {
    XLOGGER.entry(requisition);
    Profiler profiler = new Profiler("BUILD_STOCK_EVENT_FROM_REQUISITION");
    profiler.setLogger(XLOGGER);

    LOGGER.debug("Building stock events for requisition: {}", requisition.getId());

    profiler.start("GET_STOCK_CARDS");
    List<StockCardDto> stockCards = stockCardService.getStockCards(requisition.getFacilityId(),
        requisition.getProgramId()).stream().filter(stockCard -> stockCard.getLot() == null)
        .collect(Collectors.toList());

    profiler.start("BUILD_STOCK_EVENT");
    StockEventDto stockEventDto = StockEventDto
        .builder()
        .facilityId(requisition.getFacilityId())
        .programId(requisition.getProgramId())
        .userId(authenticationHelper.getCurrentUser().getId())
        .lineItems(fromLineItems(
            requisition.getRequisitionLineItems(),
            requisition.getStockAdjustmentReasons(),
            requisition.getTemplate().getColumnsMap(),
            getOccurredDate(requisition),
            stockCards
        ))
        .build();

    profiler.stop().log();
    XLOGGER.exit(stockEventDto);
    return stockEventDto;
  }

  private List<StockEventLineItemDto> fromLineItems(
      List<RequisitionLineItem> lineItems, List<StockAdjustmentReason> reasons,
      Map<String, RequisitionTemplateColumn> columnsMap, LocalDate occurredDate,
      List<StockCardDto> stockCards) {
    return lineItems.stream()
        .filter(lineItem -> !lineItem.isLineSkipped() && !lineItem.isNonFullSupply())
        .map(lineItem -> fromLineItem(lineItem, reasons, columnsMap, occurredDate, stockCards))
        .collect(Collectors.toList());
  }

  private StockEventLineItemDto fromLineItem(RequisitionLineItem lineItem,
                                             List<StockAdjustmentReason> reasons,
                                             Map<String, RequisitionTemplateColumn> columnsMap,
                                             LocalDate occurredDate,
                                             List<StockCardDto> stockCards) {
    return StockEventLineItemDto.builder()
        .orderableId(lineItem.getOrderableId())
        .quantity(lineItem.getStockOnHand() != null ? lineItem.getStockOnHand() : 0)
        .occurredDate(occurredDate)
        .stockAdjustments(getStockAdjustments(lineItem, reasons, columnsMap, stockCards))
        .build();
  }

  private List<StockEventAdjustmentDto> getStockAdjustments(RequisitionLineItem lineItem,
      List<StockAdjustmentReason> reasons, Map<String, RequisitionTemplateColumn> columnsMap,
      List<StockCardDto> stockCards) {

    XLOGGER.entry(lineItem, reasons, columnsMap, stockCards);

    Profiler profiler = new Profiler("GET_STOCK_ADJUSTMENTS");
    profiler.setLogger(XLOGGER);

    List<StockEventAdjustmentDto> stockAdjustments = new ArrayList<>();

    profiler.start("TOTAL_LOSSES_AND_ADJUSTMENTS");
    if (existsAndIsDisplayed(columnsMap.get(TOTAL_LOSSES_AND_ADJUSTMENTS))) {
      stockAdjustments = lineItem.getStockAdjustments().stream()
          .map(stockAdjustment -> fromStockAdjustment(stockAdjustment, reasons))
          .collect(Collectors.toList());
    }

    profiler.start("TOTAL_CONSUMED_QUANTITY");
    if (shouldIncludeConsumed(reasons, columnsMap)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getTotalConsumedQuantity())
          .reasonId(getReasonById(settings.getReasonIdForConsumed(), reasons))
          .build()
      );
    }

    profiler.start("TOTAL_RECEIVED_QUANTITY");
    if (shouldIncludeReceipts(reasons, columnsMap)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getTotalReceivedQuantity())
          .reasonId(getReasonById(settings.getReasonIdForReceipts(), reasons))
          .build()
      );
    }

    profiler.start("GET_STOCK_CARD_FROM_LINE_ITEM");
    StockCardDto stockCard = stockCards.stream().filter(stockCardDto -> stockCardDto.getOrderable()
        .getId().equals(lineItem.getOrderableId())).findFirst().orElse(null);

    if (stockCard == null) {
      LOGGER.warn("No stock card found for Orderable: {}", lineItem.getOrderableId());
    } else if (stockCard.getStockOnHand() == null) {
      LOGGER.warn("Stock card has no stock on hand for Orderable: {}",
              lineItem.getOrderableId());
    }

    int beginningBalance =
        lineItem.getBeginningBalance() == null ? 0 : lineItem.getBeginningBalance();

    profiler.start("INCLUDE_BEGINNING_BALANCE_EXCESS");
    if (shouldIncludeBeginningBalanceExcess(stockCard, beginningBalance, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(beginningBalance - stockCard.getStockOnHand())
          .reasonId(getReasonById(settings.getReasonIdForBeginningBalanceExcess(), reasons))
          .build());
    }

    profiler.start("INCLUDE_BEGINNING_BALANCE_INSUFFICIENCY");
    if (shouldIncludeBeginningBalanceInsufficiency(stockCard, beginningBalance, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(stockCard.getStockOnHand() - beginningBalance)
          .reasonId(getReasonById(settings.getReasonIdForBeginningBalanceInsufficiency(), reasons))
          .build());
    }

    profiler.stop().log();
    XLOGGER.exit(stockAdjustments);
    return stockAdjustments;
  }

  private StockEventAdjustmentDto fromStockAdjustment(
      StockAdjustment stockAdjustment, List<StockAdjustmentReason> reasons) {
    return StockEventAdjustmentDto.builder()
        .quantity(stockAdjustment.getQuantity())
        .reasonId(getReasonById(stockAdjustment.getReasonId(), reasons))
        .build();
  }

  private boolean shouldIncludeConsumed(List<StockAdjustmentReason> reasons,
                                        Map<String, RequisitionTemplateColumn> columnsMap) {
    return shouldInclude(
        columnsMap.get(TOTAL_CONSUMED_QUANTITY), settings.getReasonIdForConsumed(), reasons);
  }

  private boolean shouldIncludeReceipts(List<StockAdjustmentReason> reasons,
                                        Map<String, RequisitionTemplateColumn> columnsMap) {
    return shouldInclude(
        columnsMap.get(TOTAL_RECEIVED_QUANTITY), settings.getReasonIdForReceipts(), reasons);
  }

  private LocalDate getOccurredDate(Requisition requisition) {
    if (requisition.getDatePhysicalStockCountCompleted() != null) {
      return requisition.getDatePhysicalStockCountCompleted();
    } else if (requisition.getEmergency()) {
      Optional<StatusChange> submitAuditEntry = requisition.getStatusChanges().stream()
          .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.SUBMITTED)
          .findFirst();
      if (submitAuditEntry.isPresent()) {
        return submitAuditEntry.get().getCreatedDate().toLocalDate();
      }
    }
    return periodReferenceDataService.findOne(requisition.getProcessingPeriodId()).getEndDate();
  }

  private boolean shouldInclude(RequisitionTemplateColumn column, UUID reason,
                                List<StockAdjustmentReason> reasons) {
    return existsAndIsDisplayed(column) && getReasonById(reason, reasons) != null;
  }

  private boolean shouldIncludeBeginningBalanceExcess(StockCardDto stockCard,
                                                      int beginningBalance,
                                                      List<StockAdjustmentReason> reasons) {
    boolean shouldInclude = stockCard != null && stockCard.getStockOnHand() != null
        && beginningBalance > stockCard.getStockOnHand()
        && getReasonById(settings.getReasonIdForBeginningBalanceExcess(), reasons) != null;

    LOGGER.debug("Beginning balance: {}, SOH in Stock Management: {}."
                    + " Including excess adjustment: {}",
            beginningBalance,
            stockCard == null ? null : stockCard.getStockOnHand(),
            shouldInclude);

    return shouldInclude;
  }

  private boolean shouldIncludeBeginningBalanceInsufficiency(StockCardDto stockCard,
                                                             int beginningBalance,
                                                             List<StockAdjustmentReason> reasons) {
    boolean shouldInclude = stockCard != null && stockCard.getStockOnHand() != null
        && beginningBalance < stockCard.getStockOnHand()
        && getReasonById(settings.getReasonIdForBeginningBalanceInsufficiency(), reasons) != null;

    LOGGER.debug("Beginning balance: {}, SOH in Stock Management: {}."
                    + " Including insufficiency adjustment: {}",
            beginningBalance,
            stockCard == null ? null : stockCard.getStockOnHand(),
            shouldInclude);

    return shouldInclude;
  }

  private UUID getReasonById(UUID reasonId, List<StockAdjustmentReason> reasons) {
    return reasons
        .stream()
        .map(StockAdjustmentReason::getReasonId)
        .filter(id -> Objects.equals(id, reasonId))
        .findFirst()
        .orElse(null);
  }

  private boolean existsAndIsDisplayed(RequisitionTemplateColumn column) {
    return column != null && column.getIsDisplayed();
  }

}
