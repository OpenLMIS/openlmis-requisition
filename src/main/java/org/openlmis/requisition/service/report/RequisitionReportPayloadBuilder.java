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

package org.openlmis.requisition.service.report;

import java.text.NumberFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.joda.money.Money;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.UserDto;

/**
 * Flattens requisition report data into plain maps and scalars, since the payload reaches the
 * report service as JSON and domain objects would not survive the trip.
 */
public final class RequisitionReportPayloadBuilder {

  private RequisitionReportPayloadBuilder() {
    throw new UnsupportedOperationException();
  }

  /**
   * Builds the record backing the requisition report. Dates and money are formatted here because
   * the formatters cannot be sent to the report service.
   *
   * @param reportDto the requisition report data.
   * @param dateFormat pattern used for dates.
   * @param currencyFormat format used for monetary values.
   * @return map of the fields the requisition template declares.
   */
  public static Map<String, Object> buildReportRecord(RequisitionReportDto reportDto,
                                                      String dateFormat,
                                                      NumberFormat currencyFormat) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(dateFormat);
    RequisitionDto requisition = reportDto.getRequisition();
    UUID programId = requisition.getProgram().getId();

    Map<String, Object> reportRecord = new HashMap<>();
    reportRecord.put("facilityName", requisition.getFacility().getName());
    reportRecord.put("facilityCode", requisition.getFacility().getCode());
    reportRecord.put("facilityTypeName", requisition.getFacility().getType().getName());
    reportRecord.put("zoneName", requisition.getFacility().getGeographicZone().getName());
    reportRecord.put("programName", requisition.getProgram().getName());
    reportRecord.put("emergency", requisition.getEmergency());
    reportRecord.put("status", requisition.getStatus().toString());
    reportRecord.put("reportingPeriod",
        formatter.format(requisition.getProcessingPeriod().getStartDate())
            + " - " + formatter.format(requisition.getProcessingPeriod().getEndDate()));

    reportRecord.put("initiatedBy", printName(reportDto.getInitiatedBy()));
    reportRecord.put("initiatedDate", formatDate(reportDto.getInitiatedDate(), formatter));
    reportRecord.put("submittedBy", printName(reportDto.getSubmittedBy()));
    reportRecord.put("submittedDate", formatDate(reportDto.getSubmittedDate(), formatter));
    reportRecord.put("authorizedBy", printName(reportDto.getAuthorizedBy()));
    reportRecord.put("authorizedDate", formatDate(reportDto.getAuthorizedDate(), formatter));

    reportRecord.put("fullSupplyTotalCost",
        formatMoney(reportDto.getFullSupplyTotalCost(), currencyFormat));
    reportRecord.put("nonFullSupplyTotalCost",
        formatMoney(reportDto.getNonFullSupplyTotalCost(), currencyFormat));
    reportRecord.put("totalCost", formatMoney(reportDto.getTotalCost(), currencyFormat));

    reportRecord.put("fullSupply",
        buildLineItems(reportDto.getFullSupply(), programId, currencyFormat));
    reportRecord.put("nonFullSupply",
        buildLineItems(reportDto.getNonFullSupply(), programId, currencyFormat));

    return reportRecord;
  }

  private static List<Map<String, Object>> buildLineItems(List<RequisitionLineItemDto> lineItems,
                                                          UUID programId,
                                                          NumberFormat currencyFormat) {
    List<Map<String, Object>> records = new ArrayList<>();
    if (lineItems == null) {
      return records;
    }

    for (RequisitionLineItemDto lineItem : lineItems) {
      records.add(buildLineItem(lineItem, programId, currencyFormat));
    }
    return records;
  }

  @SuppressWarnings("PMD.NcssCount")
  private static Map<String, Object> buildLineItem(RequisitionLineItemDto lineItem,
                                                   UUID programId,
                                                   NumberFormat currencyFormat) {
    Map<String, Object> lineItemRecord = new HashMap<>();

    lineItemRecord.put("beginningBalance", lineItem.getBeginningBalance());
    lineItemRecord.put("totalReceivedQuantity", lineItem.getTotalReceivedQuantity());
    lineItemRecord.put("totalLossesAndAdjustments", lineItem.getTotalLossesAndAdjustments());
    lineItemRecord.put("stockOnHand", lineItem.getStockOnHand());
    lineItemRecord.put("requestedQuantity", lineItem.getRequestedQuantity());
    lineItemRecord.put("totalConsumedQuantity", lineItem.getTotalConsumedQuantity());
    lineItemRecord.put("requestedQuantityExplanation", lineItem.getRequestedQuantityExplanation());
    lineItemRecord.put("remarks", lineItem.getRemarks());
    lineItemRecord.put("approvedQuantity", lineItem.getApprovedQuantity());
    lineItemRecord.put("totalStockoutDays", lineItem.getTotalStockoutDays());
    lineItemRecord.put("total", lineItem.getTotal());
    lineItemRecord.put("packsToShip", lineItem.getPacksToShip());
    lineItemRecord.put("numberOfNewPatientsAdded", lineItem.getNumberOfNewPatientsAdded());
    lineItemRecord.put("skipped", lineItem.getSkipped());
    lineItemRecord.put("adjustedConsumption", lineItem.getAdjustedConsumption());
    lineItemRecord.put("averageConsumption", lineItem.getAverageConsumption());
    lineItemRecord.put("maximumStockQuantity", lineItem.getMaximumStockQuantity());
    lineItemRecord.put("calculatedOrderQuantity", lineItem.getCalculatedOrderQuantity());
    lineItemRecord.put("idealStockAmount", lineItem.getIdealStockAmount());
    lineItemRecord.put("calculatedOrderQuantityIsa", lineItem.getCalculatedOrderQuantityIsa());
    lineItemRecord.put("additionalQuantityRequired", lineItem.getAdditionalQuantityRequired());
    lineItemRecord.put("numberOfPatientsOnTreatmentNextMonth",
        lineItem.getNumberOfPatientsOnTreatmentNextMonth());
    lineItemRecord.put("totalRequirement", lineItem.getTotalRequirement());
    lineItemRecord.put("totalQuantityNeededByHf", lineItem.getTotalQuantityNeededByHf());
    lineItemRecord.put("quantityToIssue", lineItem.getQuantityToIssue());
    lineItemRecord.put("convertedQuantityToIssue", lineItem.getConvertedQuantityToIssue());
    lineItemRecord.put("dosesPerPatient", lineItem.getDosesPerPatient());

    lineItemRecord.put("pricePerPack", formatMoney(lineItem.getPricePerPack(), currencyFormat));
    lineItemRecord.put("totalCost", formatMoney(lineItem.getTotalCost(), currencyFormat));

    OrderableDto orderable = lineItem.getOrderable();
    if (orderable != null) {
      lineItemRecord.put("productCode", orderable.getProductCode());
      lineItemRecord.put("fullProductName", orderable.getFullProductName());
      lineItemRecord.put("netContent", orderable.getNetContent());
      lineItemRecord.put("categoryDisplayName", orderable.getProgramOrderable(programId)
          .getOrderableCategoryDisplayName());
      if (orderable.getDispensable() != null) {
        lineItemRecord.put("dispensableDisplayUnit", orderable.getDispensable().getDisplayUnit());
      }
    }

    return lineItemRecord;
  }

  private static String printName(UserDto user) {
    return user == null ? null : user.printName();
  }

  private static String formatDate(ZonedDateTime dateTime, DateTimeFormatter formatter) {
    return dateTime == null ? null : formatter.format(dateTime.toLocalDate());
  }

  private static String formatMoney(Money money, NumberFormat currencyFormat) {
    return money == null ? null : currencyFormat.format(money.getAmount());
  }
}
