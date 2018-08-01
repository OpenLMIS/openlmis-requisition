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
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FIELD_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_HIDDEN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.utils.Message;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;

public class StockOnHandValidatorTest {

  @Test
  public void shouldRejectIfValueIsLessThanZeroDuringStatusChange() {
    StockOnHandValidator validator = getStockOnHandValidator(-10);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).contains(new AbstractMap.SimpleEntry<>(
        REQUISITION_LINE_ITEMS, new Message(ERROR_MUST_BE_NON_NEGATIVE, STOCK_ON_HAND)));
  }

  @Test
  public void shouldRejectIfValueIsNullDuringStatusChange() {
    StockOnHandValidator validator = getStockOnHandValidator(null);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).contains(new AbstractMap.SimpleEntry<>(
        REQUISITION_LINE_ITEMS, new Message(ERROR_VALUE_MUST_BE_ENTERED, STOCK_ON_HAND)));
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenColumnDoesNotExist() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .setTemplate(new RequisitionTemplateDataBuilder().withAllColumnsExceptStockOnHand().build())
        .build();

    StockOnHandValidator validator =
        new StockOnHandValidator(requisition, requisition.getTemplate());

    validator.validateCanChangeStatus(new HashMap<>());
  }

  @Test
  public void shouldNotThrowExceptionWhenColumnDoesExist() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .setTemplate(new RequisitionTemplateDataBuilder().withAllColumns().build())
        .build();

    StockOnHandValidator validator =
        new StockOnHandValidator(requisition, requisition.getTemplate());

    validator.validateCanChangeStatus(new HashMap<>());
  }

  @Test
  public void shouldRejectIfColumnIsHiddenAndValueNotEmpty() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .setTemplate(new RequisitionTemplateDataBuilder().withStockOnHandColumnHiden().build())
        .build();

    StockOnHandValidator validator =
        new StockOnHandValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).contains(new AbstractMap.SimpleEntry<>(
        REQUISITION_LINE_ITEMS, new Message(ERROR_IS_HIDDEN, STOCK_ON_HAND)));
  }

  @Test
  public void shouldRejectIfStockOnHandIsIncorrectlyCalculated() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().buildWithIncorrectStockOnHand())
        .setTemplate(new RequisitionTemplateDataBuilder().withAllColumns().build())
        .build();

    StockOnHandValidator validator =
        new StockOnHandValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).contains(new AbstractMap.SimpleEntry<>(
        REQUISITION_LINE_ITEMS,
        new Message(ERROR_INCORRECT_VALUE, STOCK_ON_HAND, TOTAL_CONSUMED_QUANTITY)));
  }

  @Test
  public void shouldNotRejectIfStockOnHandIsIncorrectlyCalculatedAndPopulatedFromStockCards() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().buildWithIncorrectStockOnHand())
        .setTemplate(new RequisitionTemplateDataBuilder()
            .withAllColumns()
            .withPopulateStockOnHandFromStockCards()
            .build())
        .build();

    StockOnHandValidator validator =
        new StockOnHandValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(0);
  }

  @Test
  public void shouldNotRejectIfStockOnHandIsCorrectlyCalculated() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .setTemplate(new RequisitionTemplateDataBuilder().withAllColumns().build())
        .build();

    StockOnHandValidator validator =
        new StockOnHandValidator(requisition, requisition.getTemplate());

    Map<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldRejectIfColumnIsCalculatedAndValueNotEmpty() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .build())
        .setTemplate(new RequisitionTemplateDataBuilder().withStockOnHandColumnCalculated().build())
        .build();

    Map<String, Message> errors = new HashMap<>();
    new StockOnHandValidator(requisition, requisition.getTemplate())
        .validateCanUpdate(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors)
        .containsEntry(REQUISITION_LINE_ITEMS, new Message(ERROR_FIELD_IS_CALCULATED));
  }

  private StockOnHandValidator getStockOnHandValidator(Integer stockOnHand) {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .setStockOnHand(stockOnHand)
            .build())
        .build();

    return new StockOnHandValidator(requisition, requisition.getTemplate());
  }

}
