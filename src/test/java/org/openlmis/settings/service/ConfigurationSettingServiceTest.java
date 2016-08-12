package org.openlmis.settings.service;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@Transactional
public class ConfigurationSettingServiceTest {

  @Mock
  private ConfigurationSettingRepository configurationSettingRepository;

  @InjectMocks
  @Autowired
  private ConfigurationSettingService configurationSettingService;

  private ConfigurationSetting configurationSetting;

  @Before
  public void setUp() {
    generateInstances();
    initMocks(this);
    mockRepositories();
  }

  @Test
  public void testGetStringValue() throws ConfigurationSettingException {
    Assert.assertTrue(configurationSettingService.getStringValue("key").equals("value"));
  }

  @Test(expected = ConfigurationSettingException.class)
  public void testGetEmptyStringValue() throws ConfigurationSettingException {
    configurationSettingService.getStringValue("testEmpty");
  }

  @Test
  public void testGetEmptyBoolValue() throws ConfigurationSettingException {
    Assert.assertTrue(configurationSettingService.getBoolValue("testEmpty").equals(Boolean.FALSE));
  }

  @Test
  public void testGetBoolTrueValue() {
    configurationSetting = new ConfigurationSetting();
    configurationSetting.setKey("testTrue");
    configurationSetting.setValue(Boolean.TRUE.toString());
    when(configurationSettingRepository
            .findOne(configurationSetting.getKey()))
            .thenReturn(configurationSetting);
    Assert.assertTrue(configurationSettingService.getBoolValue("testTrue").equals(Boolean.TRUE));
  }

  @Test
  public void testGetBoolFalseValue() {
    ConfigurationSetting setting = new ConfigurationSetting();
    setting.setKey("testFalse");
    setting.setValue(Boolean.FALSE.toString());
    when(configurationSettingRepository
            .findOne(configurationSetting.getKey()))
            .thenReturn(configurationSetting);
    Assert.assertTrue(configurationSettingService.getBoolValue("testFalse").equals(Boolean.FALSE));
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
