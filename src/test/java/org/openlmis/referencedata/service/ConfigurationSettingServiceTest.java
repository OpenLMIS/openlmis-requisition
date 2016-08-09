package org.openlmis.referencedata.service;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.openlmis.settings.service.ConfigurationSettingService;

import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ConfigurationSettingServiceTest {

  @Mock
  private ConfigurationSettingRepository repository;

  @InjectMocks
  private ConfigurationSettingService service = new ConfigurationSettingService();

  private static final String TEST_STRING = "testString";

  @Test
  public void shouldGetSettingValue() throws ConfigurationSettingException {
    when(repository.findOne(TEST_STRING)).thenReturn(
            new ConfigurationSetting(TEST_STRING, "testValue"));
    Assert.assertEquals("testValue", service.getStringValue(TEST_STRING));
  }

  @Test(expected = ConfigurationSettingException.class)
  public void shouldThrowExceptionOnNonExistingSetting() throws ConfigurationSettingException {
    service.getStringValue("testEmpty");
  }

  @Test
  public void shouldReturnFalseForEmptyBooleanVal() throws ConfigurationSettingException {
    Assert.assertFalse(service.getBoolValue("testEmpty"));
  }

  @Test
  public void shouldReturnTrueBooleanValue() {
    when(repository.findOne("testTrue")).thenReturn(
            new ConfigurationSetting(TEST_STRING, "true"));
    Assert.assertTrue(service.getBoolValue("testTrue"));
  }

  @Test
  public void shouldReturnFalseBooleanValue() {
    when(repository.findOne(TEST_STRING)).thenReturn(
            new ConfigurationSetting(TEST_STRING, "false"));
    Assert.assertFalse(service.getBoolValue("testTrue"));
  }
}
