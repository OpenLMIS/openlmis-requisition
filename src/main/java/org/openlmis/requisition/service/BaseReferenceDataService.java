package org.openlmis.requisition.service;

import org.apache.commons.codec.binary.Base64;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public abstract class BaseReferenceDataService<T> {

  private static final String BASE_URL = "http://referencedata:8080/api";

  /**
   * Return one object from Reference data service.
   * @param id UUID of requesting object.
   * @return Requesting reference data object.
   */
  public T findOne(UUID id) {
    String url = BASE_URL + getUrl() + id;

    RestTemplate restTemplate = new RestTemplate();
    Map<String, String> params = new HashMap<>();
    params.put("access_token", obtainAccessToken());

    ResponseEntity<T> responseEntity = restTemplate
        .exchange(url, HttpMethod.GET, null, getResultClass(), params);

    T object = responseEntity.getBody();
    return object;
  }

  public List<T> findAll(String resourceUrl) {
    return findAll(resourceUrl, new HashMap<>());
  }

  /**
   * Return all reference data T objects.
   * @param resourceUrl Endpoint url.
   * @param parameters Map of query parameters.
   * @return all reference data T objects.
   */
  public List<T> findAll(String resourceUrl, Map<String, Object> parameters) {
    String url = BASE_URL + getUrl() + resourceUrl;
    RestTemplate restTemplate = new RestTemplate();
    Map<String, Object> params = new HashMap<>();
    params.putAll(parameters);
    ResponseEntity<List<T>> response = restTemplate.exchange(url, HttpMethod.GET,
        null, new ParameterizedTypeReference<List<T>>() {}, params);
    List<T> result = response.getBody();
    return result;
  }

  protected abstract String getUrl();

  protected abstract Class<T> getResultClass();

  private String obtainAccessToken() {
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
}