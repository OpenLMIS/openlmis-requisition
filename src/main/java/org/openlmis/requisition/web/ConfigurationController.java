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

package org.openlmis.requisition.web;

import java.util.HashMap;
import java.util.Map;

import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ConfigurationController extends BaseRequisitionController {

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  /**
   * Return unskiprequisition environment variable value.
   * @return map with unskipRequisition value
   */
  @GetMapping(RESOURCE_URL + "/unSkipRequisition")
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Map<String,Boolean> unskipRequisition() {
    Map<String,Boolean> response = new HashMap<>();
    String unskipRequisition = configurationSettingService.getUnskippingRequisition();
    response.put("unskipRequisition",Boolean.valueOf(unskipRequisition));
    return response;
  }
}
