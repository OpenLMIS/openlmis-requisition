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

package org.openlmis.requisition.domain.requisition;

import static org.apache.commons.lang3.BooleanUtils.isNotTrue;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RequisitionValidationService {

  private static final Logger logger = LoggerFactory.getLogger(RequisitionValidationService.class);

  private final Requisition savedRequisition;
  private List<RequisitionDomainValidator> validators = new ArrayList<>();

  /**
   * Constructs new requisition validation service.
   */
  public RequisitionValidationService(Requisition requisition, Requisition savedRequisition,
                                      LocalDate currentDate,
                                      boolean isDatePhysicalStockCountCompletedEnabled) {
    this.savedRequisition = savedRequisition;
    validators.add(new RequisitionInvariantsValidator(requisition, savedRequisition));
    validators.add(new ApprovalFieldsValidator(requisition, savedRequisition));
    validators.add(new StockAdjustmentReasonsValidator(requisition, savedRequisition));
    validators.add(new DatePhysicalStockCountCompletedValidator(
        requisition.getDatePhysicalStockCountCompleted(), savedRequisition, currentDate,
        isDatePhysicalStockCountCompletedEnabled));
    validators.add(new StockOnHandValidator(requisition, savedRequisition.getTemplate()));
    validators.add(new TotalConsumedQuantityValidator(requisition, savedRequisition.getTemplate()));
    validators.add(new StockOutDaysValidator(
        requisition, savedRequisition.getNumberOfMonthsInPeriod()));
  }

  /**
   * Validates if requisition can be updated. Return errors as {@link ValidationResult}.
   */
  ValidationResult validateRequisitionCanBeUpdated() {
    Map<String, Message> errors = new HashMap<>();

    for (RequisitionDomainValidator validator : validators) {
      if (!validator.isForRegularOnly()) {
        validator.validate(errors);
      }
      if (isNotTrue(savedRequisition.getEmergency()) && validator.isForRegularOnly()) {
        validator.validate(errors);
      }

    }
    if (!isEmpty(errors)) {
      logger.warn("Validation for requisition update failed: {}", errors);
      return ValidationResult.fieldErrors(errors);
    }

    return ValidationResult.success();

  }
}
