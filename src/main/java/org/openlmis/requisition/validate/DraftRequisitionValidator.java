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

import static org.openlmis.requisition.domain.Requisition.EMERGENCY;
import static org.openlmis.requisition.domain.Requisition.FACILITY_ID;
import static org.openlmis.requisition.domain.Requisition.MODIFIED_DATE;
import static org.openlmis.requisition.domain.Requisition.PROCESSING_PERIOD_ID;
import static org.openlmis.requisition.domain.Requisition.PROGRAM_ID;
import static org.openlmis.requisition.domain.Requisition.SUPERVISORY_NODE_ID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DATE_MODIFIED_MISMATCH;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FIELD_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_INVARIANT;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ONLY_AVAILABLE_FOR_APPROVAL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import java.time.ZonedDateTime;
import java.util.HashSet;
import java.util.Set;

@Component
public class DraftRequisitionValidator extends AbstractRequisitionValidator {

  private static final int DAYS_IN_MONTH = 30;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;
    Requisition savedRequisition = requisitionRepository.findOne(requisition.getId());

    validateDateModifiedIsCorrect(errors, requisition, savedRequisition);

    validateInvariantsDidntChange(errors, requisition, savedRequisition);

    if (!isEmpty(requisition.getNonSkippedFullSupplyRequisitionLineItems())) {
      requisition.getNonSkippedFullSupplyRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, savedRequisition, i));
    }
  }

  private void validateInvariantsDidntChange(Errors errors, Requisition requisition,
                                             Requisition savedRequisition) {
    rejectIfValueChanged(errors, requisition.getFacilityId(),
        savedRequisition.getFacilityId(), FACILITY_ID);
    rejectIfValueChanged(errors, requisition.getProgramId(),
        savedRequisition.getProgramId(), PROGRAM_ID);
    rejectIfValueChanged(errors, requisition.getProcessingPeriodId(),
        savedRequisition.getProcessingPeriodId(), PROCESSING_PERIOD_ID);
    rejectIfValueChanged(errors, requisition.getEmergency(),
        savedRequisition.getEmergency(), EMERGENCY);
    rejectIfValueChanged(errors, requisition.getSupervisoryNodeId(),
        savedRequisition.getSupervisoryNodeId(), SUPERVISORY_NODE_ID);
  }

  private void validateRequisitionLineItem(Errors errors, Requisition requisition,
                                           RequisitionLineItem item) {
    RequisitionTemplate template = requisition.getTemplate();
    rejectIfCalculatedAndNotNull(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);
    rejectIfCalculatedAndNotNull(errors, template, item.getTotalConsumedQuantity(),
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    rejectIfTotalStockOutDaysIsGreaterThanLengthOfPeriod(errors,
        requisition.getNumberOfMonthsInPeriod(), item);

    validateApprovalFields(errors, requisition, item);
  }

  private void validateApprovalFields(Errors errors, Requisition requisition,
                                      RequisitionLineItem item) {
    Set<RequisitionStatus> expectedStatuses = new HashSet<>();
    expectedStatuses.add(RequisitionStatus.IN_APPROVAL);
    if (configurationSettingService.getSkipAuthorization()) {
      expectedStatuses.add(RequisitionStatus.SUBMITTED);
    } else {
      expectedStatuses.add(RequisitionStatus.AUTHORIZED);
    }
    rejectIfInvalidStatusAndNotNull(errors, requisition, item.getApprovedQuantity(),
        expectedStatuses, new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
            RequisitionLineItem.APPROVED_QUANTITY));

    rejectIfInvalidStatusAndNotNull(errors, requisition, item.getRemarks(),
        expectedStatuses, new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
            RequisitionLineItem.REMARKS_COLUMN));

  }

  private void rejectIfTotalStockOutDaysIsGreaterThanLengthOfPeriod(Errors errors, int
      monthsInPeriod, RequisitionLineItem requisitionLineItem) {
    if (requisitionLineItem.getTotalStockoutDays() == null) {
      return;
    }

    int totalStockoutDays = requisitionLineItem.getTotalStockoutDays();

    if (totalStockoutDays > monthsInPeriod * DAYS_IN_MONTH) {
      rejectValue(errors, REQUISITION_LINE_ITEMS,
          new Message(ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD));
    }
  }

  private void rejectIfCalculatedAndNotNull(Errors errors, RequisitionTemplate template,
                                            Object value, String field) {
    if (template.isColumnCalculated(field) && value != null) {
      rejectValue(errors, REQUISITION_LINE_ITEMS, new Message(ERROR_FIELD_IS_CALCULATED, field));
    }
  }

  private void rejectIfInvalidStatusAndNotNull(Errors errors, Requisition requisition, Object value,
                                               Set<RequisitionStatus> expectedStatuses, Message
                                                   message) {
    if (!expectedStatuses.contains(requisition.getStatus()) && value != null) {
      rejectValue(errors, REQUISITION_LINE_ITEMS, message);
    }
  }

  private void rejectIfValueChanged(Errors errors, Object value, Object savedValue, String field) {
    if (value != null && savedValue != null && !savedValue.equals(value)) {
      rejectValue(errors, field, new Message(ERROR_IS_INVARIANT, field));
    }
  }

  private void validateDateModifiedIsCorrect(Errors errors, Requisition requisition,
                                             Requisition requisitionToUpdate) {
    ZonedDateTime dateModified = requisition.getModifiedDate();
    if (dateModified != null && !dateModified.isEqual(requisitionToUpdate.getModifiedDate())) {
      rejectValue(errors, MODIFIED_DATE, new Message(ERROR_DATE_MODIFIED_MISMATCH));
    }
  }
}
