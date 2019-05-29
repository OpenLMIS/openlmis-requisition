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
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.CALCULATED_ORDER_QUANTITY_ISA;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE;

import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import org.openlmis.requisition.utils.Message;

public class CalculatedFieldsValidatorTest {

  @Test
  public void shouldPassValidation() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .build();

    CalculatedFieldsValidator validator =
        new CalculatedFieldsValidator(requisition, requisition.getTemplate());

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldRejectIfMaximumStockQuantityIsIncorrectlyCalculated() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .withIncorrectMaximumStockQuantity()
            .build())
        .build();

    CalculatedFieldsValidator validator =
        new CalculatedFieldsValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors)
        .containsEntry(REQUISITION_LINE_ITEMS,
            new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE, MAXIMUM_STOCK_QUANTITY));
  }

  @Test
  public void shouldRejectIfCalculatedOrderQuantityIsIncorrectlyCalculated() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .withIncorrectCalculatedOrderQuantityIsa()
            .build())
        .build();

    CalculatedFieldsValidator validator =
        new CalculatedFieldsValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors)
        .containsEntry(REQUISITION_LINE_ITEMS,
            new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE, CALCULATED_ORDER_QUANTITY));
  }

  @Test
  public void shouldRejectIfCalculatedOrderQuantityIsaIsIncorrectlyCalculated() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .withIncorrectCalculatedOrderQuantityIsa()
            .build())
        .build();

    CalculatedFieldsValidator validator =
        new CalculatedFieldsValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors)
        .containsEntry(REQUISITION_LINE_ITEMS,
            new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE,
                CALCULATED_ORDER_QUANTITY_ISA));
  }

}