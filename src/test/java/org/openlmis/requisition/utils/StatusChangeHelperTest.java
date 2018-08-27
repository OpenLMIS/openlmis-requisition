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
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.requisition.dto.StatusChangeDto;

public class StatusChangeHelperTest {

  private StatusChangeDto statusChange;

  private Map<String, StatusLogEntry> statustLogEntries;

  private UUID authorId = UUID.randomUUID();
  private ZonedDateTime currentDate = ZonedDateTime.now();
  private ZonedDateTime submitDate = currentDate.minusDays(3);
  private ZonedDateTime authorizeDate = currentDate.minusDays(1);

  @Before
  public void setUp() {
    statustLogEntries = new HashMap<>();
    statustLogEntries.put(SUBMITTED.name(), new StatusLogEntry(authorId, submitDate));
    statustLogEntries.put(AUTHORIZED.name(), new StatusLogEntry(authorId, authorizeDate));

    statusChange = mock(StatusChangeDto.class);
  }

  @Test
  public void addOrUpdateShouldAddLogEntryIfItDoesNotExist() {
    when(statusChange.getCreatedDate()).thenReturn(currentDate.minusDays(1));
    when(statusChange.getStatus()).thenReturn(APPROVED);

    StatusChangeHelper.addOrUpdate(statustLogEntries, statusChange);

    assertEquals(3, statustLogEntries.size());
    assertTrue(statustLogEntries.containsKey(APPROVED.name()));
  }

  @Test
  public void addOrUpdateShouldUpdateLogEntryIfExistingOneHasLaterDate() {
    ZonedDateTime updatedDate = currentDate.minusDays(2);
    when(statusChange.getCreatedDate()).thenReturn(updatedDate);
    when(statusChange.getStatus()).thenReturn(AUTHORIZED);

    StatusChangeHelper.addOrUpdate(statustLogEntries, statusChange);

    assertEquals(2, statustLogEntries.size());
    assertEquals(updatedDate, statustLogEntries.get(AUTHORIZED.name()).getChangeDate());
  }

  @Test
  public void addOrUpdateShouldNotUpdateLogEntryIfStatusChangeOneHasLaterDate() {
    when(statusChange.getCreatedDate()).thenReturn(currentDate);
    when(statusChange.getStatus()).thenReturn(AUTHORIZED);

    StatusChangeHelper.addOrUpdate(statustLogEntries, statusChange);

    assertEquals(2, statustLogEntries.size());
    assertEquals(authorizeDate, statustLogEntries.get(AUTHORIZED.name()).getChangeDate());
  }

}