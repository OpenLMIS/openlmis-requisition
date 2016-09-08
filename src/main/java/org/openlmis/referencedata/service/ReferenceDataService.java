package org.openlmis.referencedata.service;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.exception.ExternalApiException;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;


@Service
public class  ReferenceDataService {

  public String apiUrl = "/users/";

  /**
   * Method returning a reference data object.
   *
   * @return Reference data object.
   */
  public User getReferenceDataObjectJson(UUID id, String token) {
    try {
      String url = "http://referencedata:8080/api" + apiUrl + id + "?access_token=" + token;

      RestTemplate restTemplate = new RestTemplate();
      ResponseEntity<User> responseEntity = restTemplate
          .exchange(url, HttpMethod.GET, null, User.class);
      User object =  responseEntity.getBody();
      return object;

    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not get reference data entity.", ex);
    }
  }
}
