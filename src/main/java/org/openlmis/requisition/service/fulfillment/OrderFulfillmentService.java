package org.openlmis.requisition.service.fulfillment;

import static org.openlmis.requisition.service.AuthService.ACCESS_TOKEN;

import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import java.util.List;
import java.util.UUID;

@Service
public class OrderFulfillmentService extends BaseFulfillmentService<OrderDto> {

  /**
   * Creates a new instance of order
   *
   * @param order instance that contain data required to save order
   * @return true if success, false if failed.
   */
  public boolean create(OrderDto order) {
    String url = getServiceUrl() + getUrl();

    RequestParameters parameters = RequestParameters
        .init()
        .set(ACCESS_TOKEN, obtainAccessToken());

    HttpEntity<OrderDto> body = new HttpEntity<>(order);

    try {
      restTemplate.postForEntity(createUri(url, parameters), body, Object.class);
    } catch (RestClientException ex) {
      logger.error("Can not create Order ", ex);
      return false;
    }
    return true;
  }

  public List<OrderDto> search(UUID supplyingFacility, UUID requestingFacility,
                               UUID program, UUID processingPeriod, String status) {
    RequestParameters parameters = RequestParameters
        .init()
        .set("supplyingFacility", supplyingFacility)
        .set("requestingFacility", requestingFacility)
        .set("program", program)
        .set("processingPeriod", processingPeriod)
        .set("status", status);

    return findAll("search", parameters);
  }

  public List<ProofOfDeliveryDto> getProofOfDeliveries(UUID orderId) {
    return findAll(orderId + "/proofOfDeliveries", ProofOfDeliveryDto[].class);
  }

  @Override
  protected String getUrl() {
    return "/api/orders/";
  }

  @Override
  protected Class<OrderDto> getResultClass() {
    return OrderDto.class;
  }

  @Override
  protected Class<OrderDto[]> getArrayResultClass() {
    return OrderDto[].class;
  }

}
