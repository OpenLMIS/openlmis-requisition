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
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.junit.Test;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.utils.Message;

public class TotalReceivedQuantityValidatorTest {

  @Test
  public void shouldPassValidation() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build(), false)
        .build();

    TotalReceivedQuantityValidator validator = getTotalReceivedQuantityValidator(requisition);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldRejectIfValueIsLessThanZeroDuringStatusChange() {
    TotalReceivedQuantityValidator validator = getTotalReceivedQuantityValidator(-10);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).containsEntry(REQUISITION_LINE_ITEMS,
        new Message(ERROR_MUST_BE_NON_NEGATIVE));
  }

  @Test
  public void shouldRejectIfValueIsNullDuringStatusChange() {
    TotalReceivedQuantityValidator validator = getTotalReceivedQuantityValidator((Integer) null);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(1);
    assertThat(errors).containsEntry(REQUISITION_LINE_ITEMS,
        new Message(ERROR_VALUE_MUST_BE_ENTERED));
  }

  @Test
  public void shouldNotRejectValueIfRequisitionStatusIsBeforeAuthorizedAndPatientTabIsEnabled() {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .withTotalRequirement(null)
            .build(), false)
        .build();
    requisition.getTemplate().setPatientsTabEnabled(true);
    TotalReceivedQuantityValidator validator = getTotalReceivedQuantityValidator(requisition);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanChangeStatus(errors);

    assertThat(errors).hasSize(0);
  }

  private TotalReceivedQuantityValidator getTotalReceivedQuantityValidator(
      Integer totalReceivedQuantity) {
    Requisition requisition = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder()
            .withTotalReceivedQuantity(totalReceivedQuantity)
            .build(), false)
        .build();

    return getTotalReceivedQuantityValidator(requisition);
  }

  private TotalReceivedQuantityValidator getTotalReceivedQuantityValidator(
      Requisition requisition) {
    Map<VersionIdentityDto, OrderableDto> orderables = requisition
        .getRequisitionLineItems()
        .stream()
        .map(line -> new OrderableDtoDataBuilder()
            .withId(line.getOrderable().getId())
            .withVersionNumber(line.getOrderable().getVersionNumber())
            .withProgramOrderable(requisition.getProgramId(), true)
            .buildAsDto())
        .collect(Collectors.toMap(OrderableDto::getIdentity, Function.identity()));

    return new TotalReceivedQuantityValidator(requisition,
        requisition.getTemplate(), orderables);
  }

}
