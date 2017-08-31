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

import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventAdjustmentDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventLineItemDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockCardStockManagementService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class StockEventBuilder {

  private static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  private static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  private static final String CONSUMED = "CONSUMED";
  private static final String RECEIPTS = "RECEIPTS";
  private static final String BEGINNING_BALANCE_EXCESS = "BEGINNING_BALANCE_EXCESS";
  private static final String BEGINNING_BALANCE_INSUFFICIENCY = "BEGINNING_BALANCE_INSUFFICIENCY";
  private static final String REASON_ID_SUFFIX = "_REASON_ID";

  private static final Map<String, UUID> defaultReasons = getDefaultReasons();

  private static final Logger LOGGER = LoggerFactory.getLogger(StockEventBuilder.class);

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private StockCardStockManagementService stockCardService;

  /**
   * Builds a physical inventory draft DTO from the given requisition.
   *
   * @param requisitionDto  the requisition to be used a source for the physical inventory draft
   * @return  the create physical inventory draft
   */
  public StockEventDto fromRequisition(Requisition requisitionDto) {
    List<StockCardDto> stockCards = stockCardService.getStockCards(requisitionDto.getFacilityId(),
        requisitionDto.getProgramId()).stream().filter(stockCard -> stockCard.getLot() == null)
        .collect(Collectors.toList());

    return StockEventDto
        .builder()
        .facilityId(requisitionDto.getFacilityId())
        .programId(requisitionDto.getProgramId())
        .userId(authenticationHelper.getCurrentUser().getId())
        .lineItems(fromLineItems(
            requisitionDto.getRequisitionLineItems(),
            requisitionDto.getStockAdjustmentReasons(),
            requisitionDto.getTemplate().getColumnsMap(),
            getOccurredDate(requisitionDto),
            stockCards
        ))
        .build();
  }

  private List<StockEventLineItemDto> fromLineItems(
      List<RequisitionLineItem> lineItems, List<StockAdjustmentReason> reasons,
      Map<String, RequisitionTemplateColumn> columnsMap, LocalDate occurredDate,
      List<StockCardDto> stockCards) {
    return lineItems.stream()
        .filter(lineItem -> !lineItem.getSkipped())
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
    List<StockEventAdjustmentDto> stockAdjustments = new ArrayList<>();

    if (existsAndIsDisplayed(columnsMap.get(TOTAL_LOSSES_AND_ADJUSTMENTS))) {
      stockAdjustments = lineItem.getStockAdjustments().stream()
          .map(stockAdjustment -> fromStockAdjustment(stockAdjustment, reasons))
          .collect(Collectors.toList());
    }

    if (shouldInclude(columnsMap.get(TOTAL_CONSUMED_QUANTITY), CONSUMED, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getTotalConsumedQuantity())
          .reason(getReasonById(getReasonId(CONSUMED), reasons))
          .build()
      );
    }

    if (shouldInclude(columnsMap.get(TOTAL_RECEIVED_QUANTITY), RECEIPTS, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getTotalReceivedQuantity())
          .reason(getReasonById(getReasonId(RECEIPTS), reasons))
          .build()
      );
    }

    StockCardDto stockCard = stockCards.stream().filter(stockCardDto -> stockCardDto.getOrderable()
        .getId().equals(lineItem.getOrderableId())).findFirst().orElse(null);

    if (shouldIncludeBeginningBalanceExcess(lineItem, stockCard, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getBeginningBalance() - stockCard.getStockOnHand())
          .reason(getReasonById(getReasonId(BEGINNING_BALANCE_EXCESS), reasons))
          .build());
    }

    if (shouldIncludeBeginningBalanceInsufficiency(lineItem, stockCard, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(stockCard.getStockOnHand() - lineItem.getBeginningBalance())
          .reason(getReasonById(getReasonId(BEGINNING_BALANCE_INSUFFICIENCY), reasons))
          .build());
    }

    return stockAdjustments;
  }

  private StockEventAdjustmentDto fromStockAdjustment(
      StockAdjustment stockAdjustment, List<StockAdjustmentReason> reasons) {
    return StockEventAdjustmentDto.builder()
        .quantity(stockAdjustment.getQuantity())
        .reason(getReasonById(stockAdjustment.getReasonId(), reasons))
        .build();
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

  private boolean shouldInclude(RequisitionTemplateColumn column, String reason,
                                List<StockAdjustmentReason> reasons) {
    return existsAndIsDisplayed(column) && getReasonById(getReasonId(reason), reasons) != null;
  }

  private boolean shouldIncludeBeginningBalanceExcess(RequisitionLineItem lineItem,
                                                      StockCardDto stockCard,
                                                      List<StockAdjustmentReason> reasons) {
    boolean shouldInclude = stockCard != null && stockCard.getStockOnHand() != null
        && lineItem.getBeginningBalance() > stockCard.getStockOnHand()
        && getReasonById(getReasonId(BEGINNING_BALANCE_EXCESS), reasons) != null;

    LOGGER.debug("Beginning balance: {}, SOH in Stock Management: {}."
                    + " Including excess adjustment: {}",
            lineItem.getBeginningBalance(),
            stockCard == null ? null : stockCard.getStockOnHand(),
            shouldInclude);

    return shouldInclude;
  }

  private boolean shouldIncludeBeginningBalanceInsufficiency(RequisitionLineItem lineItem,
                                                             StockCardDto stockCard,
                                                             List<StockAdjustmentReason> reasons) {
    boolean shouldInclude = stockCard != null && stockCard.getStockOnHand() != null
        && lineItem.getBeginningBalance() < stockCard.getStockOnHand()
        && getReasonById(getReasonId(BEGINNING_BALANCE_INSUFFICIENCY), reasons) != null;

    LOGGER.debug("Beginning balance: {}, SOH in Stock Management: {}."
                    + " Including insufficiency adjustment: {}",
            lineItem.getBeginningBalance(),
            stockCard == null ? null : stockCard.getStockOnHand(),
            shouldInclude);

    return shouldInclude;
  }

  private ReasonDto getReasonById(String reasonId, List<StockAdjustmentReason> reasons) {
    return reasonId != null ? getReasonById(UUID.fromString(reasonId), reasons) : null;
  }

  private ReasonDto getReasonById(UUID reasonId, List<StockAdjustmentReason> reasons) {
    return ReasonDto.newInstance(reasons.stream()
        .filter(reasonDto -> reasonDto.getReasonId().equals(reasonId))
        .findFirst().orElse(null));
  }

  private boolean existsAndIsDisplayed(RequisitionTemplateColumn column) {
    return column != null && column.getIsDisplayed();
  }

  private UUID getReasonId(String reason) {
    String reasonId = System.getenv(reason + REASON_ID_SUFFIX);
    return reasonId != null ? UUID.fromString(reasonId) : defaultReasons.get(reason);
  }

  private static Map<String, UUID> getDefaultReasons() {
    Map<String, UUID> defaultReasons = new HashMap<>();

    defaultReasons.put(CONSUMED, UUID.fromString("b5c27da7-bdda-4790-925a-9484c5dfb594"));
    defaultReasons.put(RECEIPTS, UUID.fromString("313f2f5f-0c22-4626-8c49-3554ef763de3"));
    defaultReasons.put(BEGINNING_BALANCE_EXCESS,
        UUID.fromString("84eb13c3-3e54-4687-8a5f-a9f20dcd0dac"));
    defaultReasons.put(BEGINNING_BALANCE_INSUFFICIENCY,
        UUID.fromString("f8bb41e2-ab43-4781-ae7a-7bf3b5116b82"));

    return defaultReasons;
  }

}
