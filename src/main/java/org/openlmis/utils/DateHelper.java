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

package org.openlmis.utils;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import java.time.LocalDate;
import java.time.ZoneId;

@Component
public class DateHelper {

  @Value("${time.zoneId}")
  private String timeZoneId;

  /**
   * Checks if startDate is before the current date from the system clock in the system time zone.
   * @return true if startDate is before now or is null
   */
  public boolean isStartDateBeforeNow(LocalDate startDate) {
    return (startDate == null) || startDate.isBefore(getCurrentDateWithSystemZone());
  }

  /**
   * Obtains the current date from the system clock in the system time zone.
   * @return the current date using the system clock
   */
  public LocalDate getCurrentDateWithSystemZone() {
    return LocalDate.now(ZoneId.of(timeZoneId));
  }

}
