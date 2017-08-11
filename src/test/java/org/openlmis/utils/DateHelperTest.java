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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;

@RunWith(MockitoJUnitRunner.class)
public class DateHelperTest {

  public static final String UTC = "UTC";
  public static final ZoneId ZONE_ID = ZoneId.of(UTC);

  @Mock
  Clock clock;

  @InjectMocks
  DateHelper dateHelper;

  @Before
  public void setUp() {
    when(clock.withZone(ZONE_ID)).thenReturn(clock);
    when(clock.getZone()).thenReturn(ZONE_ID);
    when(clock.instant()).thenReturn(Instant.now());

    ReflectionTestUtils.setField(dateHelper, "timeZoneId", UTC);
  }

  @Test
  public void shouldGetCurrentDateWithSystemTimeZone() {
    assertEquals(LocalDate.now(clock.withZone(ZONE_ID)), dateHelper.getCurrentDateWithSystemZone());
  }

  @Test
  public void shouldReturnTrueWhenStartDateIsBeforeNow() {
    boolean startDateBeforeNow =
        dateHelper.isStartDateBeforeNow(LocalDate.now(clock.withZone(ZONE_ID)).minusDays(1));

    assertTrue(startDateBeforeNow);
  }

  @Test
  public void shouldReturnFalseWhenStartDateIsAfterNow() {
    boolean startDateBeforeNow =
        dateHelper.isStartDateBeforeNow(LocalDate.now(clock.withZone(ZONE_ID)).plusDays(1));

    assertFalse(startDateBeforeNow);
  }

  @Test
  public void shouldReturnFalseWhenStartDateEqualsNow() {
    boolean startDateBeforeNow =
        dateHelper.isStartDateBeforeNow(LocalDate.now(clock.withZone(ZONE_ID)));

    assertFalse(startDateBeforeNow);
  }

  @Test
  public void shouldReturnTrueWhenStartDateIsNull() {
    boolean startDateBeforeNow = dateHelper.isStartDateBeforeNow(null);

    assertTrue(startDateBeforeNow);
  }

}