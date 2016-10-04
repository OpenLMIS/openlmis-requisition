package org.openlmis.requisition.service.referencedata;

import org.apache.commons.codec.binary.Base64;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public abstract class BaseReferenceDataService<T> {

  private static final String ACCESS_TOKEN = "access_token";

  @Value("${auth.server.clientId}")
  private String clientId;

  @Value("${auth.server.clientSecret}")
  private String clientSecret;

  @Value("${referencedata.url}")
  private String referenceDataUrl;

  @Value("${auth.server.authorizationUrl}")
  private String authorizationUrl;

  /**
   * Return one object from Reference data service.
   *
   * @param id UUID of requesting object.
   * @return Requesting reference data object.
   */
  public T findOne(UUID id) {
    String url = getReferenceDataUrl() + getUrl() + id;

    RestTemplate restTemplate = new RestTemplate();
    Map<String, String> params = new HashMap<>();
    params.put(ACCESS_TOKEN, obtainAccessToken());

    ResponseEntity<T> responseEntity = restTemplate
        .exchange(url, HttpMethod.GET, null, getResultClass(), params);

    return responseEntity.getBody();
  }

  /**
   * Return one object from Reference data service.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @return Requesting reference data object.
   */
  public T findOne(String resourceUrl, Map<String, Object> parameters) {
    String url = getReferenceDataUrl() + getUrl() + resourceUrl;
    RestTemplate restTemplate = new RestTemplate();
    Map<String, Object> params = new HashMap<>();
    params.putAll(parameters);
    params.put(ACCESS_TOKEN, obtainAccessToken());

    ResponseEntity<T> response = restTemplate.exchange(url, HttpMethod.GET,
          null, getResultClass(), params);

    return response.getBody();
  }

  public Collection<T> findAll() {
    return findAll("", new HashMap<>());
  }

  public Collection<T> findAll(String resourceUrl) {
    return findAll(resourceUrl, new HashMap<>());
  }

  /**
   * Return all reference data T objects.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @return all reference data T objects.
   */
  public Collection<T> findAll(String resourceUrl, Map<String, Object> parameters) {
    String url = getReferenceDataUrl() + getUrl() + resourceUrl;
    RestTemplate restTemplate = new RestTemplate();
    Map<String, Object> params = new HashMap<>();
    params.putAll(parameters);
    params.put(ACCESS_TOKEN, obtainAccessToken());

    ResponseEntity<T[]> responseEntity =
        restTemplate.getForEntity(url, getArrayResultClass(), params);

    return new ArrayList<>(Arrays.asList(responseEntity.getBody()));
  }

  protected abstract String getUrl();

  protected abstract Class<T> getResultClass();

  protected abstract Class<T[]> getArrayResultClass();

  protected String getReferenceDataUrl() {
    return referenceDataUrl;
  }

  private String obtainAccessToken() {
    RestTemplate restTemplate = new RestTemplate();

    String plainCreds = clientId + ":" + clientSecret;
    byte[] plainCredsBytes = plainCreds.getBytes();
    byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
    String base64Creds = new String(base64CredsBytes);

    HttpHeaders headers = new HttpHeaders();
    headers.add("Authorization", "Basic " + base64Creds);

    HttpEntity<String> request = new HttpEntity<>(headers);

    Map<String, Object> params = new HashMap<>();
    params.put("grant_type", "client_credentials");

    ResponseEntity<?> response = restTemplate.exchange(
        buildUri(authorizationUrl, params), HttpMethod.POST, request, Object.class);


    return ((Map<String, String>) response.getBody()).get(ACCESS_TOKEN);
  }

  private URI buildUri(String url, Map<String, ?> params) {
    UriComponentsBuilder builder = UriComponentsBuilder.newInstance().uri(URI.create(url));

    params.entrySet().forEach(e -> builder.queryParam(e.getKey(), e.getValue()));

    return builder.build(true).toUri();
  }
}
