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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.errorhandling.FailureType;
import org.openlmis.requisition.errorhandling.ValidationResult;

import java.time.ZonedDateTime;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionVersionValidatorTest {

  private RequisitionVersionValidator requisitionVersionValidator
      = new RequisitionVersionValidator();

  @Mock
  private Requisition incomingReq;

  @Mock
  private Requisition existingReq;

  @Test
  public void shouldAcceptCorrectModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();

    ValidationResult result =
        testModifiedDateValidation(dateModified, ZonedDateTime.from(dateModified));
    assertTrue(result.isSuccess());
  }

  @Test
  public void shouldAcceptWithNoModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();

    ValidationResult result = testModifiedDateValidation(null, dateModified);
    assertTrue(result.isSuccess());
  }

  @Test
  public void shouldRejectFutureModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();
    ZonedDateTime incomingDate = dateModified.plusYears(1);

    ValidationResult result = testModifiedDateValidation(incomingDate, dateModified);
    assertTrue(result.hasErrors());
    assertEquals(FailureType.CONFLICT, result.getError().getType());
  }

  @Test
  public void shouldRejectPastModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();
    ZonedDateTime incomingDate = dateModified.minusMinutes(5);

    ValidationResult result = testModifiedDateValidation(incomingDate, dateModified);
    assertTrue(result.hasErrors());
    assertEquals(FailureType.CONFLICT, result.getError().getType());
  }

  private ValidationResult testModifiedDateValidation(ZonedDateTime incomingDate, ZonedDateTime
      databaseDate) {
    when(incomingReq.getModifiedDate()).thenReturn(incomingDate);
    when(existingReq.getModifiedDate()).thenReturn(databaseDate);
    return requisitionVersionValidator.validateRequisitionTimestamps(
        incomingReq, existingReq);
  }
}
