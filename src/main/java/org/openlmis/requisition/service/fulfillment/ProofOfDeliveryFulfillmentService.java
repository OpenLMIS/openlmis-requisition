package org.openlmis.requisition.service.fulfillment;

import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class ProofOfDeliveryFulfillmentService extends BaseCommunicationService {

  @Value("${fulfillment.url}")
  private String fulfillmentUrl;

  /**
   * Return Proof of delivery based on external id.
   */
  public Collection<ProofOfDeliveryDto> findByExternalId(UUID externalId) {
    String url = fulfillmentUrl + "/api/proofOfDeliveries/search";

    Map<String, String> params = new HashMap<>();
    params.put(ACCESS_TOKEN, obtainAccessToken());

    if (null != externalId) {
      params.put("externalId", externalId.toString());
    }

    ResponseEntity<ProofOfDeliveryDto[]> responseEntity = restTemplate
        .getForEntity(buildUri(url, params), ProofOfDeliveryDto[].class);

    return new ArrayList<>(Arrays.asList(responseEntity.getBody()));
  }

}
