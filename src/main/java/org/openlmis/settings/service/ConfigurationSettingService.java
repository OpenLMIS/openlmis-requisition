package org.openlmis.settings.service;

import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import lombok.NoArgsConstructor;

@Service
@NoArgsConstructor
public class ConfigurationSettingService {

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  /**
   * Return cofiguration setting with given key.
   *
   * @param key String value of key.
   * @return Configuration setting containing given key.
   * @throws ConfigurationSettingException Exception saying that setting was not found.
   */
  public ConfigurationSetting getByKey(String key) throws ConfigurationSettingException {
    ConfigurationSetting setting = configurationSettingRepository.findOne(key);
    if (setting == null) {
      throw new ConfigurationSettingException("Configuration setting '" + key + "' not found");
    }
    return setting;
  }

  /**
   * Return value for given key if possible.
   *
   * @param key String value indicates key.
   * @return String value of given key.
   */
  public String getStringValue(String key) throws ConfigurationSettingException {
    ConfigurationSetting configurationSetting = configurationSettingRepository.findOne(key);
    if (configurationSetting == null || configurationSetting.getValue() == null) {
      throw new ConfigurationSettingException("Configuration setting '" + key + "' not found");
    }
    return configurationSetting.getValue();
  }

  /**
   * Return boolean value for given key.
   * If does not exist return false.
   *
   * @param key String value indicates key.
   * @return Boolean value of given key.
   */
  public Boolean getBoolValue(String key) {
    try {
      String value = getStringValue(key);
      return Boolean.parseBoolean(value);
    } catch (ConfigurationSettingException exception) {
      return false;
    }
  }
}
