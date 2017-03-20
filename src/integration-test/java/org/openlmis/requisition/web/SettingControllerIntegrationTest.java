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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

public class SettingControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "api/settings";
  private static final String KEY_URL = RESOURCE_URL + "/{key}";
  private static final String KEY_PARAM = "key";

  @MockBean
  private ConfigurationSettingRepository configurationSettingRepository;

  // GET /api/settings/{key}

  @Before
  public void setUp() {
    mockUserAuthenticated();
  }

  @Test
  public void shouldReturnSettingWithExistingKey() {
    // given
    String key = "randomKey";
    String value = "randomValue";
    ConfigurationSetting setting = generateSetting(key, value);

    // when
    ConfigurationSetting result = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam(KEY_PARAM, setting.getKey())
        .when()
        .get(KEY_URL)
        .then()
        .statusCode(200)
        .extract().as(ConfigurationSetting.class);

    // then
    assertEquals(key, result.getKey());
    assertEquals(value, result.getValue());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotReturnSettingWithNonExistentKey() {
    // given
    given(configurationSettingRepository.findOne(any(String.class))).willReturn(null);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam(KEY_PARAM, "emptyKey")
        .when()
        .get(KEY_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // Helper methods

  private ConfigurationSetting generateSetting(String key, String value) {
    ConfigurationSetting setting = new ConfigurationSetting(key, value);

    given(configurationSettingRepository.findOne(key)).willReturn(setting);

    return setting;
  }
}
