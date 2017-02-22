/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.service.fulfillment;

import static org.openlmis.requisition.service.AuthService.ACCESS_TOKEN;
import static org.openlmis.utils.RequestHelper.createUri;

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
        .set(ACCESS_TOKEN, authService.obtainAccessToken());

    HttpEntity<OrderDto> body = new HttpEntity<>(order);

    try {
      restTemplate.postForEntity(createUri(url, parameters), body, Object.class);
    } catch (RestClientException ex) {
      logger.error("Can not create Order ", ex);
      return false;
    }
    return true;
  }

  /**
   * Finds orders that matched the provided parameters.
   */
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

  /**
   * Finds proof of delivery related with the given order.
   */
  public ProofOfDeliveryDto getProofOfDelivery(UUID orderId) {
    return findOne(
        orderId + "/proofOfDeliveries",
        RequestParameters.init(),
        ProofOfDeliveryDto.class
    );
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
