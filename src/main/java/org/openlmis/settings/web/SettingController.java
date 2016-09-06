package org.openlmis.settings.web;

import org.openlmis.referencedata.web.BaseController;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class SettingController extends BaseController{

  @Autowired
  ConfigurationSettingService configurationSettingService;

  /**
   * Returns setting with given key.
   * @param key Key of setting to be returned.
   * @return Configuration setting with given key.
   */
  @RequestMapping(value = "/settings/{key}",  method = RequestMethod.GET)
  public ResponseEntity<?> getByKey(@PathVariable(value = "key") String key) {
    try {
      ConfigurationSetting setting = configurationSettingService.getByKey(key);
      return new ResponseEntity<>(setting, HttpStatus.OK);
    } catch (ConfigurationSettingException ex) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }
  }
}
