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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertFalse;

import org.junit.Test;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.springframework.test.util.ReflectionTestUtils;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

public class StatusChangeValidationServiceTest {

  private Requisition requisition = new RequisitionDataBuilder()
      .addLineItem(new RequisitionLineItemDataBuilder().buildForInitiatedRegularRequisition())
      .buildInitiatedRegularRequisition();
  private Class[] expectedClasses = {
      RequisitionInvariantsValidator.class,
      ApprovalFieldsValidator.class,
      DatePhysicalStockCountCompletedValidator.class,
      StockOnHandValidator.class,
      TotalConsumedQuantityValidator.class,
      StockOutDaysValidator.class,
      BeginningBalanceValidator.class,
      CalculatedFieldsValidator.class,
      NumberOfNewPatientsAddedValidator.class,
      RequestedQuantityValidator.class,
      StockAdjustmentsValidator.class,
      TotalFieldValidator.class,
      TotalReceivedQuantityValidator.class
  };

  @Test
  public void shouldPassValidations() {
    StatusChangeValidationService statusChangeValidationService =
        new StatusChangeValidationService(requisition, LocalDate.now(), true);

    ValidationResult validationResult =
        statusChangeValidationService.validateRequisitionCanChangeStatus();

    assertFalse(validationResult.hasErrors());
  }

  @Test
  public void shouldUseAllRequiredValidators() {
    StatusChangeValidationService statusChangeValidationService =
        new StatusChangeValidationService(requisition, LocalDate.now(), true);

    List<RequisitionStatusChangeDomainValidator> validators =
        (List<RequisitionStatusChangeDomainValidator>)
            ReflectionTestUtils.getField(statusChangeValidationService, "validators");

    List<Class> actual = validators.stream()
            .map(RequisitionStatusChangeDomainValidator::getClass)
            .collect(Collectors.toList());
    assertThat(actual).containsExactlyInAnyOrder(expectedClasses);

  }

}