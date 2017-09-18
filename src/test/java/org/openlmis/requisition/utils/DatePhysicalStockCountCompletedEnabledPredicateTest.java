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

package org.openlmis.requisition.utils;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class DatePhysicalStockCountCompletedEnabledPredicateTest {

  @InjectMocks
  private DatePhysicalStockCountCompletedEnabledPredicate predicate;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private ProgramDto program;

  @Before
  public void setUp() {
    when(programReferenceDataService.findOne(any(UUID.class))).thenReturn(program);
  }

  @Test
  public void shouldReturnFalseIfProgramIdIsNull() {
    assertEquals(false, predicate.exec(null));
  }

  @Test
  public void shouldReturnFalseIfProgramWasNotFound() {
    when(programReferenceDataService.findOne(any(UUID.class))).thenReturn(null);
    assertEquals(false, predicate.exec(UUID.randomUUID()));
  }

  @Test
  public void shouldReturnFalseIfProgramFieldIsNull() {
    when(program.getEnableDatePhysicalStockCountCompleted()).thenReturn(null);
    assertEquals(false, predicate.exec(UUID.randomUUID()));
  }

  @Test
  public void shouldReturnValueOfProgramField() {
    when(program.getEnableDatePhysicalStockCountCompleted()).thenReturn(true);
    assertEquals(true, predicate.exec(UUID.randomUUID()));

    when(program.getEnableDatePhysicalStockCountCompleted()).thenReturn(false);
    assertEquals(false, predicate.exec(UUID.randomUUID()));
  }

}
