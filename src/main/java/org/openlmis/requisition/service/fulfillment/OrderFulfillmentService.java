package org.openlmis.requisition.service.fulfillment;

import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.Map;

@Service
public class OrderFulfillmentService extends BaseCommunicationService {
  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Value("${fulfillment.url}")
  private String fulfillmentUrl;

  public OrderFulfillmentService() {
    this(new RestTemplate());
  }

  OrderFulfillmentService(RestTemplate restTemplate) {
    this.restTemplate = restTemplate;
  }

  /**
   * Creates a new instance of order
   *
   * @param order instance that contain data required to save order
   * @return true if success, false if failed.
   */
  public boolean create(OrderDto order) {
    String url = fulfillmentUrl + "/api/orders/";

    Map<String, String> params = new HashMap<>();
    params.put(ACCESS_TOKEN, obtainAccessToken());
    HttpEntity<OrderDto> body = new HttpEntity<>(order);

    try {
      restTemplate.postForObject(buildUri(url, params), body, OrderDto.class);
    } catch (RestClientException ex) {
      logger.error("Can not create Order ", ex);
      return false;
    }
    return true;
  }

}
