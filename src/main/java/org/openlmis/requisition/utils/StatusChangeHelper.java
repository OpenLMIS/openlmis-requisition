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

import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.requisition.dto.StatusChangeDto;
import java.util.Map;

public class StatusChangeHelper {

  /**
   * Adds or updates the given status change to the given maps status log entries. It will only
   * update the entry if the existing on has later date.
   *
   * @param statusLogEntries  the map of status go entries.
   * @param statusChange  the status change
   */
  public static void addOrUpdate(Map<String, StatusLogEntry> statusLogEntries,
                                 StatusChangeDto statusChange) {
    StatusLogEntry existing = statusLogEntries.get(statusChange.getStatus().toString());
    // Only add entry if none exists or existing one has later date
    if (existing == null || existing.getChangeDate().isAfter(statusChange.getCreatedDate())) {
      StatusLogEntry entry = new StatusLogEntry(statusChange.getAuthorId(),
          statusChange.getCreatedDate());
      statusLogEntries.put(statusChange.getStatus().toString(), entry);
    }
  }

}
