package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class ConfigurationSettingServiceTest {

  @Autowired
  private ConfigurationSettingRepository repository;

  @Autowired
  private ConfigurationSettingService service;

  @Before
  public void setUp() {
    repository.deleteAll();
  }

  @After
  public void cleanUp() {
    repository.deleteAll();
  }

  @Test
  public void testGetStringValue() {
    ConfigurationSetting setting = new ConfigurationSetting();
    setting.setKey("testString");
    setting.setValue("testValue");
    repository.save(setting);
    Assert.assertTrue(service.getStringValue("testString").equals("testValue"));
  }

  @Test
  public void testGetEmptyValue() {
    Assert.assertTrue(service.getStringValue("testEmpty").equals(""));
    Assert.assertTrue(service.getBoolValue("testEmpty").equals(Boolean.FALSE));
  }

  @Test
  public void testGetBoolTrueValue() {
    ConfigurationSetting setting = new ConfigurationSetting();
    setting.setKey("testTrue");
    setting.setValue(Boolean.TRUE.toString());
    repository.save(setting);
    Assert.assertTrue(service.getBoolValue("testTrue").equals(Boolean.TRUE));
  }

  @Test
  public void testGetBoolFalseValue() {
    ConfigurationSetting setting = new ConfigurationSetting();
    setting.setKey("testFalse");
    setting.setValue(Boolean.FALSE.toString());
    repository.save(setting);
    Assert.assertTrue(service.getBoolValue("testFalse").equals(Boolean.FALSE));
  }
}
