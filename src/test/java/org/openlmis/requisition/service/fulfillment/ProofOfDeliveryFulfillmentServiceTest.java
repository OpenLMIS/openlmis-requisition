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

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.utils.DynamicPageTypeReference;
import org.springframework.http.HttpMethod;
import java.net.URI;
import java.util.List;
import java.util.UUID;

public class ProofOfDeliveryFulfillmentServiceTest
    extends BaseFulfillmentServiceTest<ProofOfDeliveryDto> {

  @Override
  protected ProofOfDeliveryDto generateInstance() {
    return new ProofOfDeliveryDto();
  }

  @Override
  protected BaseCommunicationService<ProofOfDeliveryDto> getService() {
    return new ProofOfDeliveryFulfillmentService();
  }

  @Test
  public void shouldGetProofOfDeliveries() {
    // given
    final UUID shipmentId = UUID.randomUUID();

    ProofOfDeliveryDto pod = mockPageResponseEntityAndGetDto();

    // when
    ProofOfDeliveryFulfillmentService service =
        (ProofOfDeliveryFulfillmentService) prepareService();
    List<ProofOfDeliveryDto> actual = service.getProofOfDeliveries(shipmentId);

    // then
    assertThat(actual, hasSize(1));
    assertThat(actual, contains(pod));

    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET),
        entityCaptor.capture(), any(DynamicPageTypeReference.class)
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl();

    assertThat(uri.toString(), startsWith(url));
    assertThat(uri.toString(), containsString("shipmentId=" + shipmentId));

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }

}
