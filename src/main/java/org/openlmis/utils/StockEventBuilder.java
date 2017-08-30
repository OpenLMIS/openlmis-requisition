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
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventAdjustmentDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventLineItemDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class StockEventBuilder {

  private static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  private static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  private static final String CONSUMED_REASON_ID = "CONSUMED_REASON_ID";
  private static final String RECEIPTS_REASON_ID = "RECEIPTS_REASON_ID";

  @Autowired
  private DateHelper dateHelper;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  /**
   * Builds a physical inventory draft DTO from the given requisition.
   *
   * @param requisitionDto  the requisition to be used a source for the physical inventory draft
   * @return  the create physical inventory draft
   */
  public StockEventDto fromRequisition(Requisition requisitionDto) {
    ZonedDateTime occurredDate = getOccurredDate(requisitionDto);
    return StockEventDto
        .builder()
        .facilityId(requisitionDto.getFacilityId())
        .programId(requisitionDto.getProgramId())
        .lineItems(fromLineItems(
            requisitionDto.getRequisitionLineItems(),
            requisitionDto.getStockAdjustmentReasons(),
            requisitionDto.getTemplate().getColumnsMap(),
            occurredDate
        ))
        .build();
  }

  private List<StockEventLineItemDto> fromLineItems(
      List<RequisitionLineItem> lineItems, List<StockAdjustmentReason> reasons,
      Map<String, RequisitionTemplateColumn> columnsMap, ZonedDateTime occurredDate) {
    return lineItems.stream()
        .filter(lineItem -> !lineItem.getSkipped())
        .map(lineItem -> fromLineItem(lineItem, reasons, columnsMap, occurredDate))
        .collect(Collectors.toList());
  }

  private StockEventLineItemDto fromLineItem(RequisitionLineItem lineItem,
                                             List<StockAdjustmentReason> reasons,
                                             Map<String, RequisitionTemplateColumn> columnsMap,
                                             ZonedDateTime occurredDate) {
    return StockEventLineItemDto.builder()
        .orderableId(lineItem.getOrderableId())
        .quantity(lineItem.getStockOnHand() != null ? lineItem.getStockOnHand() : 0)
        .occurredDate(occurredDate)
        .stockAdjustments(getStockAdjustments(lineItem, reasons, columnsMap))
        .build();
  }

  private List<StockEventAdjustmentDto> getStockAdjustments(RequisitionLineItem lineItem,
      List<StockAdjustmentReason> reasons, Map<String, RequisitionTemplateColumn> columnsMap) {
    List<StockEventAdjustmentDto> stockAdjustments = new ArrayList<>();

    if (existsAndIsDisplayed(columnsMap.get(TOTAL_LOSSES_AND_ADJUSTMENTS))) {
      stockAdjustments = lineItem.getStockAdjustments().stream()
          .map(stockAdjustment -> fromStockAdjustment(stockAdjustment, reasons))
          .collect(Collectors.toList());
    }

    if (shouldInclude(columnsMap.get(TOTAL_CONSUMED_QUANTITY), CONSUMED_REASON_ID, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getTotalConsumedQuantity())
          .reason(getReasonById(UUID.fromString(System.getenv(CONSUMED_REASON_ID)), reasons))
          .build()
      );
    }

    if (shouldInclude(columnsMap.get(TOTAL_RECEIVED_QUANTITY), RECEIPTS_REASON_ID, reasons)) {
      stockAdjustments.add(StockEventAdjustmentDto.builder()
          .quantity(lineItem.getTotalReceivedQuantity())
          .reason(getReasonById(UUID.fromString(System.getenv(RECEIPTS_REASON_ID)), reasons))
          .build()
      );
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

  private ZonedDateTime getOccurredDate(Requisition requisition) {
    if (requisition.getDatePhysicalStockCountCompleted() != null) {
      return requisition.getDatePhysicalStockCountCompleted().atStartOfDay(dateHelper.getZone());
    }
    return periodReferenceDataService.findOne(requisition.getProcessingPeriodId()).getEndDate()
        .atStartOfDay(dateHelper.getZone());
  }

  private boolean shouldInclude(RequisitionTemplateColumn column, String reasonKey,
                                List<StockAdjustmentReason> reasons) {
    String reasonId = System.getenv(reasonKey);
    UUID reasonUuid = null;

    if (reasonId != null) {
      reasonUuid = UUID.fromString(reasonId);
    }

    return existsAndIsDisplayed(column)
        && reasonUuid != null
        && getReasonById(reasonUuid, reasons) != null;
  }

  private ReasonDto getReasonById(UUID reasonId, List<StockAdjustmentReason> reasons) {
    List<ReasonDto> filtered = ReasonDto.newInstance(reasons.stream()
        .filter(reasonDto -> reasonDto.getReasonId().equals(reasonId))
        .collect(Collectors.toList()));
    return filtered.size() > 0 ? filtered.get(0) : null;
  }

  private boolean existsAndIsDisplayed(RequisitionTemplateColumn column) {
    return column != null && column.getIsDisplayed();
  }

}
