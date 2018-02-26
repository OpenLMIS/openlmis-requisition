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
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_STOCKOUT_DAYS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

import org.junit.Test;
import org.openlmis.requisition.utils.Message;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;

public class StockOutDaysValidatorTest {

  @Test
  public void shouldPassValidation() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .build();

    StockOutDaysValidator validator = new StockOutDaysValidator(requisition, 3,
        requisition.getTemplate());

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldRejectIfValueIsLessThanZeroDuringStatusChange() {
    StockOutDaysValidator validator = getStockOutDaysValidatorWithStockOutDays(-10);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).contains(new AbstractMap.SimpleEntry<>(
        REQUISITION_LINE_ITEMS, new Message(ERROR_MUST_BE_NON_NEGATIVE, TOTAL_STOCKOUT_DAYS)));
  }

  @Test
  public void shouldRejectIfValueIsNullDuringStatusChange() {
    StockOutDaysValidator validator = getStockOutDaysValidatorWithStockOutDays(null);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).contains(new AbstractMap.SimpleEntry<>(
        REQUISITION_LINE_ITEMS, new Message(ERROR_VALUE_MUST_BE_ENTERED, TOTAL_STOCKOUT_DAYS)));
  }

  @Test
  public void shouldRejectIfNumberOfTotalStockoutDaysIsGreaterThanLengthOfPeriod() {
    StockOutDaysValidator validator = getStockOutDaysValidatorWithStockOutDays(91);

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).containsEntry(REQUISITION_LINE_ITEMS,
        new Message(ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD));
  }

  @Test
  public void shouldNotRejectIfNumberOfTotalStockoutDaysIsLessThanLengthOfPeriod() {
    StockOutDaysValidator validator = getStockOutDaysValidatorWithStockOutDays(89);

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).isEmpty();
  }

  private StockOutDaysValidator getStockOutDaysValidatorWithStockOutDays(
      Integer totalStockoutDays) {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .setTotalStockoutDays(totalStockoutDays)
            .build())
        .build();

    return new StockOutDaysValidator(requisition, 3,
        requisition.getTemplate());
  }

}