package org.openlmis.referencedata.service;

import org.apache.commons.codec.binary.Base64;
import org.openlmis.hierarchyandsupervision.exception.ExternalApiException;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class  ReferenceDataService {

  private static final String BASE_URL = "http://referencedata:8080/api";
  private static final String API_URL = "/users/";

  /**
   * Method returning a reference data object.
   *
   * @return Reference data object.
   */
  public UserDto getReferenceDataObjectJson(UUID id, String token) {
    try {
      String url = BASE_URL + API_URL + id + "?access_token=" + token;

      RestTemplate restTemplate = new RestTemplate();
      ResponseEntity<UserDto> responseEntity = restTemplate
          .exchange(url, HttpMethod.GET, null, UserDto.class);
      UserDto object =  responseEntity.getBody();
      return object;

    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not get reference data entity.", ex);
    }
  }

  /**
   * Method returning a reference data object.
   *
   * @return Reference data object.
   */
  public UserDto findOneUser(UUID userId) {
    String url = BASE_URL + API_URL + userId;

    RestTemplate restTemplate = new RestTemplate();
    Map<String, String> params = new HashMap<String, String>();
    params.put("access_token", obtainAccessToken());

    ResponseEntity<UserDto> responseEntity = restTemplate
        .exchange(url, HttpMethod.GET, null, UserDto.class, params);

    UserDto object = responseEntity.getBody();
    return object;
  }


  /**
   * Method returning a reference data objects.
   *
   * @return Reference data object.
   */
  public UserDto[]  findAllUsers() {
    String url = BASE_URL + API_URL;

    RestTemplate restTemplate = new RestTemplate();
    Map<String, String> params = new HashMap<String, String>();
    params.put("access_token", obtainAccessToken());

    final UserDto[] data = restTemplate
        .getForObject(url, UserDto[].class, params);

    return data;
  }

  /**
   * Method saving a reference data object.
   */
  public void saveUser(UserDto user) {
    String url = BASE_URL + API_URL;
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    HttpEntity entity = new HttpEntity(user,headers);

    restTemplate.exchange(url, HttpMethod.POST, entity, UserDto.class);
  }

  /**
   * Method deleting a reference data object.
   */
  public void deleteUser(UUID userId) {
    String url = BASE_URL + API_URL + userId;

    RestTemplate restTemplate = new RestTemplate();
    restTemplate.exchange(url, HttpMethod.DELETE, null, UserDto.class);
  }

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
