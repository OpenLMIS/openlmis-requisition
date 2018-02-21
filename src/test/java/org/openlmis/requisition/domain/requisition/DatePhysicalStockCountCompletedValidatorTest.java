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

import org.junit.Test;
import org.openlmis.requisition.testutils.RequisitionDataBuilder;
import org.openlmis.requisition.utils.Message;
import java.time.LocalDate;
import java.util.HashMap;

public class DatePhysicalStockCountCompletedValidatorTest {

  private LocalDate now = LocalDate.now();

  @Test
  public void shouldRejectUpdateValidationsIfDateDifferAfterAuthorize() {
    DatePhysicalStockCountCompletedValidator validator =
        getValidator(now.minusDays(2), now.minusDays(1), RequisitionStatus.AUTHORIZED, true);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).hasSize(1);
  }

  @Test
  public void shouldNotRejectUpdateValidationsIfDateDifferBeforeAuthorize() {
    DatePhysicalStockCountCompletedValidator validator =
        getValidator(now.minusDays(2), now.minusDays(1), RequisitionStatus.INITIATED, true);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldNotRejectUpdateValidationsIfDateDifferAfterAuthorizeWhenFlagDisabled() {
    DatePhysicalStockCountCompletedValidator validator =
        getValidator(now.minusDays(2), now.minusDays(1), RequisitionStatus.AUTHORIZED, false);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldNotRejectUpdateValidationsIfDateIsTheSameAfterAuthorize() {
    DatePhysicalStockCountCompletedValidator validator =
        getValidator(now.minusDays(1), now.minusDays(1), RequisitionStatus.AUTHORIZED, true);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).isEmpty();
  }

  @Test
  public void shouldRejectUpdateValidationsIfDateIsInFuture() {
    DatePhysicalStockCountCompletedValidator validator =
        getValidator(now.minusDays(1), now.plusDays(1), RequisitionStatus.INITIATED, true);

    HashMap<String, Message> errors = new HashMap<>();
    validator.validateCanUpdate(errors);

    assertThat(errors).hasSize(1);
  }

  private DatePhysicalStockCountCompletedValidator getValidator(LocalDate oldDate,
                                                                LocalDate newDate,
                                                                RequisitionStatus reqStatus,
                                                                boolean isFlagEnabled) {
    return new DatePhysicalStockCountCompletedValidator(
        new DatePhysicalStockCountCompleted(newDate),
        new RequisitionDataBuilder()
            .setDatePhysicalStockCountCompleted(oldDate)
            .setStatus(reqStatus)
            .build(),
        now,
        isFlagEnabled);
  }

}