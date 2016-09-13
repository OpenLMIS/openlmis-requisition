package org.openlmis.referencedata.service;

import org.apache.commons.codec.binary.Base64;
import org.openlmis.hierarchyandsupervision.exception.ExternalApiException;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.oauth2.client.DefaultOAuth2ClientContext;
import org.springframework.security.oauth2.client.OAuth2RestTemplate;
import org.springframework.security.oauth2.client.token.DefaultAccessTokenRequest;
import org.springframework.security.oauth2.client.token.grant.password.ResourceOwnerPasswordResourceDetails;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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

  /**
   * This method currently shouldnt be public wtf.
   * @return current token.
   */
  public String obtainAccessToken() {
    ResourceOwnerPasswordResourceDetails resource = new ResourceOwnerPasswordResourceDetails();
    List scopes = new ArrayList<String>(2);
    scopes.add("write");
    scopes.add("read");
    resource.setAccessTokenUri("auth:8080/oauth/token");
    resource.setClientId("trusted-client");
    resource.setClientSecret("secret");
    resource.setGrantType("password");
    resource.setScope(scopes);

    resource.setUsername("admin");
    resource.setPassword("password");

    OAuth2RestTemplate restTemplate = new OAuth2RestTemplate(
        resource, new DefaultOAuth2ClientContext(new DefaultAccessTokenRequest()));
    return restTemplate.getAccessToken().toString();
  }

  private HttpHeaders createHeaders(String username, String password ) {
    return new HttpHeaders() {
      {
        String auth = username + ":" + password;
        byte[] encodedAuth = Base64.encodeBase64(
            auth.getBytes(Charset.forName("US-ASCII")) );
        String authHeader = "Basic " + new String( encodedAuth );
        set( "Authorization", authHeader );
      }
    };
  }




}
