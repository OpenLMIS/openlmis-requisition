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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.joda.money.Money;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
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

  private static final String COLUMN_KEY_PREFIX = "report.column.requisition.";

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

    Map<String, Object> record = new HashMap<>();
    record.put("facilityName", requisition.getFacility().getName());
    record.put("facilityCode", requisition.getFacility().getCode());
    record.put("facilityTypeName", requisition.getFacility().getType().getName());
    record.put("zoneName", requisition.getFacility().getGeographicZone().getName());
    record.put("programName", requisition.getProgram().getName());
    record.put("emergency", requisition.getEmergency());
    record.put("status", requisition.getStatus().toString());
    record.put("reportingPeriod",
        formatter.format(requisition.getProcessingPeriod().getStartDate())
            + " - " + formatter.format(requisition.getProcessingPeriod().getEndDate()));

    record.put("initiatedBy", printName(reportDto.getInitiatedBy()));
    record.put("initiatedDate", formatDate(reportDto.getInitiatedDate(), formatter));
    record.put("submittedBy", printName(reportDto.getSubmittedBy()));
    record.put("submittedDate", formatDate(reportDto.getSubmittedDate(), formatter));
    record.put("authorizedBy", printName(reportDto.getAuthorizedBy()));
    record.put("authorizedDate", formatDate(reportDto.getAuthorizedDate(), formatter));

    record.put("fullSupplyTotalCost",
        formatMoney(reportDto.getFullSupplyTotalCost(), currencyFormat));
    record.put("nonFullSupplyTotalCost",
        formatMoney(reportDto.getNonFullSupplyTotalCost(), currencyFormat));
    record.put("totalCost", formatMoney(reportDto.getTotalCost(), currencyFormat));

    record.put("fullSupply", buildLineItems(reportDto.getFullSupply(), programId, currencyFormat));
    record.put("nonFullSupply",
        buildLineItems(reportDto.getNonFullSupply(), programId, currencyFormat));

    return record;
  }

  /**
   * Builds the label the line item subreport falls back to for each column header.
   *
   * @param columns the columns that will be printed, in display order.
   * @return column key to configured label.
   */
  public static Map<String, String> buildColumnLabels(
      Map<String, RequisitionTemplateColumn> columns) {
    Map<String, String> labels = new LinkedHashMap<>();
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columns.entrySet()) {
      labels.put(entry.getKey(), entry.getValue().getLabel());
    }
    return labels;
  }

  /**
   * Builds the translation key for each column header. A column is only keyed while its label is
   * still the shipped default, so a label an administrator renamed is printed as entered.
   *
   * @param columns the columns that will be printed, in display order.
   * @return column key to translation key, for columns with an untouched label.
   */
  public static Map<String, String> buildColumnLabelKeys(
      Map<String, RequisitionTemplateColumn> columns) {
    Map<String, String> keys = new LinkedHashMap<>();
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columns.entrySet()) {
      RequisitionTemplateColumn column = entry.getValue();
      if (usesShippedLabel(column)) {
        keys.put(entry.getKey(), COLUMN_KEY_PREFIX + column.getName());
      }
    }
    return keys;
  }

  private static boolean usesShippedLabel(RequisitionTemplateColumn column) {
    return column.getColumnDefinition() != null
        && column.getLabel() != null
        && column.getLabel().equals(column.getColumnDefinition().getLabel());
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
    Map<String, Object> record = new HashMap<>();

    record.put("beginningBalance", lineItem.getBeginningBalance());
    record.put("totalReceivedQuantity", lineItem.getTotalReceivedQuantity());
    record.put("totalLossesAndAdjustments", lineItem.getTotalLossesAndAdjustments());
    record.put("stockOnHand", lineItem.getStockOnHand());
    record.put("requestedQuantity", lineItem.getRequestedQuantity());
    record.put("totalConsumedQuantity", lineItem.getTotalConsumedQuantity());
    record.put("requestedQuantityExplanation", lineItem.getRequestedQuantityExplanation());
    record.put("remarks", lineItem.getRemarks());
    record.put("approvedQuantity", lineItem.getApprovedQuantity());
    record.put("totalStockoutDays", lineItem.getTotalStockoutDays());
    record.put("total", lineItem.getTotal());
    record.put("packsToShip", lineItem.getPacksToShip());
    record.put("numberOfNewPatientsAdded", lineItem.getNumberOfNewPatientsAdded());
    record.put("skipped", lineItem.getSkipped());
    record.put("adjustedConsumption", lineItem.getAdjustedConsumption());
    record.put("averageConsumption", lineItem.getAverageConsumption());
    record.put("maximumStockQuantity", lineItem.getMaximumStockQuantity());
    record.put("calculatedOrderQuantity", lineItem.getCalculatedOrderQuantity());
    record.put("idealStockAmount", lineItem.getIdealStockAmount());
    record.put("calculatedOrderQuantityIsa", lineItem.getCalculatedOrderQuantityIsa());
    record.put("additionalQuantityRequired", lineItem.getAdditionalQuantityRequired());
    record.put("numberOfPatientsOnTreatmentNextMonth",
        lineItem.getNumberOfPatientsOnTreatmentNextMonth());
    record.put("totalRequirement", lineItem.getTotalRequirement());
    record.put("totalQuantityNeededByHf", lineItem.getTotalQuantityNeededByHf());
    record.put("quantityToIssue", lineItem.getQuantityToIssue());
    record.put("convertedQuantityToIssue", lineItem.getConvertedQuantityToIssue());
    record.put("dosesPerPatient", lineItem.getDosesPerPatient());

    record.put("pricePerPack", formatMoney(lineItem.getPricePerPack(), currencyFormat));
    record.put("totalCost", formatMoney(lineItem.getTotalCost(), currencyFormat));

    OrderableDto orderable = lineItem.getOrderable();
    if (orderable != null) {
      record.put("productCode", orderable.getProductCode());
      record.put("fullProductName", orderable.getFullProductName());
      record.put("netContent", orderable.getNetContent());
      record.put("categoryDisplayName", orderable.getProgramOrderable(programId)
          .getOrderableCategoryDisplayName());
      if (orderable.getDispensable() != null) {
        record.put("dispensableDisplayUnit", orderable.getDispensable().getDisplayUnit());
      }
    }

    return record;
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
