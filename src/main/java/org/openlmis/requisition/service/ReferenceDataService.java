package org.openlmis.requisition.service;

import org.apache.commons.codec.binary.Base64;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;


@Service
public class  ReferenceDataService {

  private static final String BASE_URL = "http://referencedata:8080/api";
  private static final String USERS_URL = "/users/";
  private static final String SUPPLY_LINES_URL = "/supplyLines/";
  private static final String PERIODS_URL = "/processingPeriods/";

  /**
   * Method returning a reference data object.
   *
   * @return Reference data object.
   */
  public UserDto findUser(UUID userId) {
    String url = BASE_URL + USERS_URL + userId;

    RestTemplate restTemplate = new RestTemplate();
    Map<String, String> params = new HashMap<String, String>();
    params.put("access_token", obtainAccessToken());

    ResponseEntity<UserDto> responseEntity = restTemplate
        .exchange(url, HttpMethod.GET, null, UserDto.class, params);

    UserDto object = responseEntity.getBody();
    return object;
  }

  /**
   *
   * @param programId
   * @param supervisoryNodeId
   * @return
   */
  public List<SupplyLineDto> searchSupplyLines(UUID programId, UUID supervisoryNodeId) {
    String url = BASE_URL + SUPPLY_LINES_URL + "searchByUUID";
    RestTemplate restTemplate = new RestTemplate();
    Map<String, UUID> params = new HashMap<>();
    params.put("programId", programId);
    params.put("supervisoryNodeId", supervisoryNodeId);
    ResponseEntity<List<SupplyLineDto>> response = restTemplate.exchange(url, HttpMethod.GET,
        null, new ParameterizedTypeReference<List<SupplyLineDto>>() {}, params);
    List<SupplyLineDto> result = response.getBody();
    return result;
  }

  /**
   *
   * @param processingScheduleId
   * @param startDate
   * @return
   */
  public Iterable<ProcessingPeriodDto> searchPeriods(UUID processingScheduleId, LocalDate startDate) {
    String url = BASE_URL + PERIODS_URL + "searchByUUIDAndDate";
    RestTemplate restTemplate = new RestTemplate();
    Map<String, Object> params = new HashMap<>();
    params.put("processingScheduleId", processingScheduleId);
    params.put("startDate", startDate);
    ResponseEntity<List<ProcessingPeriodDto>> response = restTemplate.exchange(url, HttpMethod.GET,
        null, new ParameterizedTypeReference<List<ProcessingPeriodDto>>() {}, params);
    List<ProcessingPeriodDto> result = response.getBody();
    return result;
  }

  /**
   * This method currently shouldnt be public wtf.
   * @return current token.
   */
  public String obtainAccessToken() {
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