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

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

@SuppressWarnings("PMD.TooManyMethods")
public class SettingControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "api/settings";
  private static final String KEY_URL = RESOURCE_URL + "/{key}";
  private static final String key = "key";
  private static final String value = "value";

  private ConfigurationSetting setting = new ConfigurationSetting();

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Before
  public void setUp() {
    setting.setKey(key);
    setting.setValue(value);
    configurationSettingRepository.save(setting);
  }

  @Test
  public void shouldReturnSettingWithExistingKey() {

    ConfigurationSetting response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("key", key)
        .when()
        .get(KEY_URL)
        .then()
        .statusCode(200)
        .extract().as(ConfigurationSetting.class);

    assertTrue(configurationSettingRepository.exists(response.getKey()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotReturnSettingWithNonexistentKey() {

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("key", "emptyKey")
        .when()
        .get(KEY_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
