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

package org.openlmis.settings.service;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class ConfigurationSettingServiceTest {

  @InjectMocks
  private ConfigurationSettingService configurationSettingService;

  @Test
  public void shouldReturnSkipAuthorizationAsTrueIfTrue() {
    // given
    ReflectionTestUtils.setField(configurationSettingService, "skipAuthorizationSetting", "true");

    // when
    boolean result = configurationSettingService.getSkipAuthorization();

    // then
    assertTrue(result);
  }

  @Test
  public void shouldReturnSkipAuthorizationAsFalseIfNotTrue() {
    // given
    ReflectionTestUtils.setField(configurationSettingService, "skipAuthorizationSetting", "false");

    // when
    boolean result = configurationSettingService.getSkipAuthorization();

    // then
    assertFalse(result);
  }
}
