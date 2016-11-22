package org.openlmis.requisition.service.fulfillment;


import org.apache.commons.codec.binary.Base64;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.service.InternalOrderService;
import org.openlmis.requisition.dto.OrderDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

@Service
public class OrderFulfillmentService {
  private static final String ACCESS_TOKEN = "access_token";

  private RestTemplate restTemplate;

  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Autowired
  private InternalOrderService internalOrderService;

  @Value("${auth.server.clientId}")
  private String clientId;

  @Value("${auth.server.clientSecret}")
  private String clientSecret;

  @Value("${fulfillment.url}")
  private String fulfillmentUrl;

  @Value("${auth.server.authorizationUrl}")
  private String authorizationUrl;

  public OrderFulfillmentService() {
    this(new RestTemplate());
  }

  OrderFulfillmentService(RestTemplate restTemplate) {
    this.restTemplate = restTemplate;
  }

  /**
   * Saves a new instance of order.
   *
   * @param orderDto instance that contain data required to save order
   */
  public void save(OrderDto orderDto) {
    internalOrderService.save(Order.newOrder(orderDto));
  }

  /**
   * Creates a new instance of order
   * @param orderDto instance that contain data required to save order
   * @return true if success, false if failed.
   */
  public boolean create(OrderDto orderDto) {
    String url = fulfillmentUrl + "/api/orders/";

    Map<String, String> params = new HashMap<>();
    params.put(ACCESS_TOKEN, obtainAccessToken());
    Order order = Order.newOrder(orderDto);
    HttpEntity<Order> body = new HttpEntity<>(order);

    try {
      restTemplate.postForObject(buildUri(url, params), body, Order.class);
    } catch (RestClientException ex) {
      logger.error("Can not create Order ");
      return false;
    }
    return true;
  }

  private String obtainAccessToken() {
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
