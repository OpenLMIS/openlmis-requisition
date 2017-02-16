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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.repository.ConfigurationSettingRepository;

@RunWith(MockitoJUnitRunner.class)
public class ConfigurationSettingServiceTest {

  @Mock
  private ConfigurationSettingRepository configurationSettingRepository;

  @InjectMocks
  private ConfigurationSettingService configurationSettingService;

  private ConfigurationSetting configurationSetting;

  @Before
  public void setUp() {
    generateInstances();
    mockRepositories();
  }

  @Test
  public void shouldGetConfigurationSettingByKeyIfKeyExists() throws ConfigurationSettingException {
    assertEquals(configurationSettingService.getByKey("key"), configurationSetting);
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldGetConfigurationSettingByKeyIfDoesNotKeyExists()
      throws ContentNotFoundMessageException {
    configurationSettingService.getByKey("testEmpty");
  }

  @Test
  public void shouldGetValueIfKeyExists() throws ConfigurationSettingException {
    assertEquals(configurationSettingService.getStringValue("key"), "value");
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldThrowExceptionIfKeyDoesNotExists() throws ContentNotFoundMessageException {
    configurationSettingService.getStringValue("testEmpty");
  }

  @Test
  public void shouldCatchExceptionAndReturnFalseIfKeyDoesNotExists()
      throws ConfigurationSettingException {
    assertEquals(configurationSettingService.getBoolValue("testEmpty"), Boolean.FALSE);
  }

  @Test
  public void shouldGetBoolTrueValueIfKeyExists() {
    configurationSetting = new ConfigurationSetting();
    configurationSetting.setKey("testTrue");
    configurationSetting.setValue(Boolean.TRUE.toString());
    when(configurationSettingRepository
        .findOne(configurationSetting.getKey()))
        .thenReturn(configurationSetting);
    assertEquals(configurationSettingService.getBoolValue("testTrue"), Boolean.TRUE);
  }

  @Test
  public void shouldGetBoolFalseValueIfKeyExists() {
    ConfigurationSetting setting = new ConfigurationSetting();
    setting.setKey("testFalse");
    setting.setValue(Boolean.FALSE.toString());
    when(configurationSettingRepository
        .findOne(configurationSetting.getKey()))
        .thenReturn(configurationSetting);
    assertEquals(configurationSettingService.getBoolValue("testFalse"), Boolean.FALSE);
  }

  private void generateInstances() {
    configurationSetting = new ConfigurationSetting();
    configurationSetting.setKey("key");
    configurationSetting.setValue("value");
  }

  private void mockRepositories() {
    when(configurationSettingRepository
        .findOne(configurationSetting.getKey()))
        .thenReturn(configurationSetting);
  }
}
