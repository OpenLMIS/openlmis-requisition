package org.openlmis.requisition.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class SettingControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "api/settings";
  private static final String KEY_URL = RESOURCE_URL + "/{key}";
  private static final String ACCESS_TOKEN = "access_token";
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
