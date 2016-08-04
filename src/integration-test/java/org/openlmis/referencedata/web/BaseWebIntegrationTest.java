package org.openlmis.referencedata.web;

import org.apache.commons.codec.binary.Base64;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.util.Map;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
@Transactional
public abstract class BaseWebIntegrationTest {
  static final String BASE_URL = System.getenv("BASE_URL");

  private String token = null;

  private String fetchToken() {
    RestTemplate restTemplate = new RestTemplate();

    String plainCreds = "trusted-client:secret";
    byte[] plainCredsBytes = plainCreds.getBytes();
    byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
    String base64Creds = new String(base64CredsBytes);

    HttpHeaders headers = new HttpHeaders();
    headers.add("Authorization", "Basic " + base64Creds);

    HttpEntity<String> request = new HttpEntity<>(headers);
    ResponseEntity<?> response = restTemplate.exchange(
        "http://auth:8080/oauth/token?grant_type=password&username=admin&password=password",
        HttpMethod.POST, request, Object.class);

    return ((Map<String, String>) response.getBody()).get("access_token");
  }

  String getToken() {
    if (token == null) {
      token = fetchToken();
    }
    return token;
  }

  String addTokenToUrl(String url) {
    return url + "?access_token=" + this.getToken();
  }
}
