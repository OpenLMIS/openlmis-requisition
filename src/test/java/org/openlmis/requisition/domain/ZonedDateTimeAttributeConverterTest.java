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

package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;

import java.sql.Timestamp;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.TimeZone;
import org.apache.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ZonedDateTimeAttributeConverterTest {

  private static final Logger LOGGER = Logger.getLogger(ZonedDateTimeAttributeConverterTest.class);
  
  private String[] zoneIdStrings = TimeZone.getAvailableIDs();
  private TimeZone systemTimeZone;
  
  private ZonedDateTimeAttributeConverter converter;

  @Before
  public void setUp() {
    systemTimeZone = TimeZone.getDefault();
    
    converter = new ZonedDateTimeAttributeConverter();
  }
  
  @After
  public void cleanUp() {
    TimeZone.setDefault(systemTimeZone);
  }
  
  @Test
  public void convertToDatabaseColumnShouldConvertWithAllTimezones() {
    for (String zoneIdString : zoneIdStrings) {
      TimeZone.setDefault(TimeZone.getTimeZone(zoneIdString));
      ZonedDateTime zonedDateTime = ZonedDateTime.now();

      Timestamp sqlTimestamp = converter.convertToDatabaseColumn(zonedDateTime);

      LOGGER.debug("zonedDateTime = " + zonedDateTime.toString()
          + ", sqlTimestamp = " + sqlTimestamp.toString());
      ZonedDateTime sqlZdt = ZonedDateTime.of(sqlTimestamp.toLocalDateTime(), ZoneId.of("UTC"));
      LOGGER.debug("zonedDateTime = " + zonedDateTime.toString()
          + ", sqlZdt = " + sqlZdt.toString());
      LOGGER.debug("zonedDateTime instant = " + zonedDateTime.toInstant().toString()
          + ", sqlZdt instant = " + sqlZdt.toInstant().toString());
      assertEquals(zonedDateTime.toInstant(), sqlZdt.toInstant());
    }
  }

  @Test
  public void convertToEntityAttributeShouldConvertWithAllTimezones() {
    for (String zoneIdString : zoneIdStrings) {
      TimeZone.setDefault(TimeZone.getTimeZone(zoneIdString));
      Timestamp sqlTimestamp = Timestamp.from(Instant.now());

      ZonedDateTime zonedDateTime = converter.convertToEntityAttribute(sqlTimestamp);

      LOGGER.debug("sqlTimestamp = " + sqlTimestamp.toString()
          + ", zonedDateTime = " + zonedDateTime.toString());
      ZonedDateTime sqlZdt = ZonedDateTime.of(sqlTimestamp.toLocalDateTime(), ZoneId.of("UTC"));
      LOGGER.debug("sqlZdt instant = " + sqlZdt.toInstant().toString()
          + ", zonedDateTime instant = " + zonedDateTime.toInstant().toString());
      assertEquals(sqlZdt.toInstant(), zonedDateTime.toInstant());
    }
  }
}
