package org.openlmis.settings.service;

import lombok.NoArgsConstructor;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@NoArgsConstructor
public class ConfigurationSettingService {

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  public String getStringValue(String key) {
    ConfigurationSetting configurationSetting = configurationSettingRepository.findOne(key);
    if (configurationSetting == null || configurationSetting.getValue() == null) {
      return "";
    }
    return configurationSetting.getValue();
  }

  public Boolean getBoolValue(String key) {
    String value = getStringValue(key);
    return Boolean.parseBoolean(value);
  }
}
