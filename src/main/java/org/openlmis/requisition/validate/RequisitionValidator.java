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

package org.openlmis.requisition.validate;

import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateCalculatedOrderQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateMaximumStockQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateStockOnHand;
import static org.openlmis.requisition.domain.Requisition.DATE_PHYSICAL_STOCK_COUNT_COMPLETED;
import static org.openlmis.requisition.domain.RequisitionLineItem.APPROVED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.BEGINNING_BALANCE;
import static org.openlmis.requisition.domain.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.NUMBER_OF_NEW_PATIENTS_ADDED;
import static org.openlmis.requisition.domain.RequisitionLineItem.REQUESTED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION;
import static org.openlmis.requisition.domain.RequisitionLineItem.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_COLUMN;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_RECEIVED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_STOCKOUT_DAYS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_ADJUSTMENT_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class RequisitionValidator extends AbstractRequisitionValidator {
  static final String STOCK_ADJUSTMENT_REASON = "reasonId";
  static final String VALUE_IS_INCORRECTLY_CALCULATED = " has incorrect value, it does not match"
      + " the calculated value.";
  static final String VALUE_NOT_FOUND = " could not be found.";

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;

    if (isEmpty(requisition.getNonSkippedRequisitionLineItems())) {
      rejectValue(errors, REQUISITION_LINE_ITEMS,
          new Message(ERROR_VALUE_MUST_BE_ENTERED, REQUISITION_LINE_ITEMS));
    } else {
      for (RequisitionLineItem item : requisition.getNonSkippedRequisitionLineItems()) {
        if (item.isNonFullSupply()) {
          validateNonFullSupplyLineItem(errors, requisition, item);
        } else {
          validateFullSupplyLineItem(errors, requisition, item);
        }
      }
    }

    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());

    if (program.getEnableDatePhysicalStockCountCompleted()) {
      validateDatePhysicalStockCountCompleted(errors, requisition);
    }
  }

  private void validateNonFullSupplyLineItem(Errors errors, Requisition requisition,
                                             RequisitionLineItem item) {
    RequisitionTemplate template = requisition.getTemplate();

    rejectIfNullOrNegative(errors, template, item.getRequestedQuantity(),
        REQUESTED_QUANTITY);

    checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
        REQUESTED_QUANTITY_EXPLANATION);

    validateApprovedQuantity(errors, template, requisition, item);
  }

  private void validateFullSupplyLineItem(Errors errors, Requisition requisition,
                                          RequisitionLineItem item) {
    RequisitionTemplate template = requisition.getTemplate();

    rejectIfNullOrNegative(errors, template, item.getBeginningBalance(),
        BEGINNING_BALANCE);

    rejectIfNullOrNegative(errors, template, item.getTotalReceivedQuantity(),
        TOTAL_RECEIVED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getStockOnHand(),
        STOCK_ON_HAND);

    rejectIfNullOrNegative(errors, template, item.getTotalConsumedQuantity(),
        TOTAL_CONSUMED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getTotalStockoutDays(),
        TOTAL_STOCKOUT_DAYS);

    rejectIfNullOrNegative(errors, template, item.getTotal(), TOTAL_COLUMN);

    validateApprovedQuantity(errors, template, requisition, item);

    checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
        REQUESTED_QUANTITY_EXPLANATION);

    rejectIfLessThanZero(errors, template, item.getNumberOfNewPatientsAdded(),
        NUMBER_OF_NEW_PATIENTS_ADDED);

    validateCalculations(errors, template, item);

    validateRequestedQuantityAndExplanation(errors, item, template);

    validateStockAdjustments(errors, requisition, item);
  }

  private void validateRequestedQuantityAndExplanation(Errors errors, RequisitionLineItem item,
      RequisitionTemplate template) {
    rejectIfLessThanZero(errors, template, item.getRequestedQuantity(),
        REQUESTED_QUANTITY);

    if (template.isColumnDisplayed(CALCULATED_ORDER_QUANTITY)) {
      if (template.isColumnDisplayed(REQUESTED_QUANTITY)) {
        if (item.getRequestedQuantity() != null
            && !Objects.equals(item.getRequestedQuantity(), item.getCalculatedOrderQuantity())) {
          rejectIfEmpty(errors, template, item.getRequestedQuantityExplanation(),
              REQUESTED_QUANTITY_EXPLANATION);
        }
      } else {
        checkTemplate(errors, template, item.getRequestedQuantity(), REQUESTED_QUANTITY);
        checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
            REQUESTED_QUANTITY_EXPLANATION);
      }
    } else {
      rejectIfNull(errors, template, item.getRequestedQuantity(), REQUESTED_QUANTITY);
    }
  }

  private void validateStockAdjustments(
      Errors errors, Requisition requisition, RequisitionLineItem item) {
    List<UUID> reasons = stockAdjustmentReasonReferenceDataService
        .getStockAdjustmentReasonsByProgram(requisition.getProgramId())
        .stream().map(StockAdjustmentReasonDto::getId).collect(Collectors.toList());

    for (StockAdjustment adjustment : item.getStockAdjustments()) {
      if (!reasons.contains(adjustment.getReasonId())) {
        rejectValue(errors, STOCK_ADJUSTMENT_REASON,
            new Message(ERROR_STOCK_ADJUSTMENT_NOT_FOUND, adjustment.getReasonId()));
      }

      if (adjustment.getQuantity() == null || adjustment.getQuantity() < 0) {
        rejectValue(errors, STOCK_ADJUSTMENT_REASON,
            new Message(ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE, adjustment.getReasonId()));
      }
    }
  }

  private void validateApprovedQuantity(Errors errors, RequisitionTemplate template,
                                        Requisition requisition, RequisitionLineItem item) {
    if (requisition.isApprovable()
        || (configurationSettingService.getSkipAuthorization()
        && requisition.getStatus() == RequisitionStatus.SUBMITTED)) {

      rejectIfNull(errors, template, item.getApprovedQuantity(),
          APPROVED_QUANTITY);
      rejectIfLessThanZero(errors, template, item.getApprovedQuantity(),
          APPROVED_QUANTITY);
    }
  }

  private void validateCalculations(Errors errors, RequisitionTemplate template,
                                    RequisitionLineItem item) {
    boolean templateValid = checkTemplate(errors, template, item.getStockOnHand(),
        STOCK_ON_HAND) && checkTemplate(errors, template,
        item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);

    if (templateValid && !Objects.equals(item.getStockOnHand(), calculateStockOnHand(item))) {
      rejectValue(errors, REQUISITION_LINE_ITEMS,
          new Message(ERROR_INCORRECT_VALUE, STOCK_ON_HAND, TOTAL_CONSUMED_QUANTITY));
    }

    if (checkTemplate(errors, template, item.getMaximumStockQuantity(), MAXIMUM_STOCK_QUANTITY)
        && !Objects.equals(item.getMaximumStockQuantity(), calculateMaximumStockQuantity(item,
        template))) {
      rejectValue(errors, REQUISITION_LINE_ITEMS,
          new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE, MAXIMUM_STOCK_QUANTITY));
    }

    if (checkTemplate(errors, template, item.getCalculatedOrderQuantity(),
        CALCULATED_ORDER_QUANTITY) && !Objects.equals(item.getCalculatedOrderQuantity(),
        calculateCalculatedOrderQuantity(item, template))) {
      rejectValue(errors, REQUISITION_LINE_ITEMS,
          new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE, CALCULATED_ORDER_QUANTITY));
    }
  }

  private void validateDatePhysicalStockCountCompleted(Errors errors, Requisition requisition) {
    if ((requisition.isPreAuthorize()
        && requisition.getDatePhysicalStockCountCompleted() == null)) {
      rejectValue(errors, DATE_PHYSICAL_STOCK_COUNT_COMPLETED,
          new Message(ERROR_VALUE_MUST_BE_ENTERED, DATE_PHYSICAL_STOCK_COUNT_COMPLETED));
    }
  }
}
