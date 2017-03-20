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

package org.openlmis.settings.web;

import org.openlmis.requisition.web.BaseController;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class SettingController extends BaseController {

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  /**
   * Returns setting with given key.
   *
   * @param key Key of setting to be returned.
   * @return Configuration setting with given key.
   */
  @RequestMapping(value = "/settings/{key}", method = RequestMethod.GET)
  public ResponseEntity<?> getByKey(@PathVariable(value = "key") String key) {
    ConfigurationSetting setting = configurationSettingService.getByKey(key);
    return new ResponseEntity<>(setting, HttpStatus.OK);
  }
}
