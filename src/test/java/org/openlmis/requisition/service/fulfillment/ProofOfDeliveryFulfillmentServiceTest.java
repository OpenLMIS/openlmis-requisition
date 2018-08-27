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
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;

import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.BaseCommunicationService;

public class ProofOfDeliveryFulfillmentServiceTest
    extends BaseFulfillmentServiceTest<ProofOfDeliveryDto> {

  private ProofOfDeliveryFulfillmentService service;

  @Override
  protected ProofOfDeliveryDto generateInstance() {
    return new ProofOfDeliveryDto();
  }

  @Override
  protected BaseCommunicationService<ProofOfDeliveryDto> getService() {
    return new ProofOfDeliveryFulfillmentService();
  }

  @Override
  @Before
  public void setUp() {
    super.setUp();
    service = (ProofOfDeliveryFulfillmentService) prepareService();
  }

  @Test
  public void shouldGetProofOfDeliveries() {
    // given
    UUID shipmentId = UUID.randomUUID();

    // when
    ProofOfDeliveryDto pod = mockPageResponseEntityAndGetDto();
    List<ProofOfDeliveryDto> actual = service.getProofOfDeliveries(shipmentId);

    // then
    assertThat(actual, hasSize(1));
    assertThat(actual, contains(pod));

    verifyPageRequest()
        .isGetRequest()
        .hasAuthHeader()
        .hasEmptyBody()
        .isUriStartsWith(service.getServiceUrl() + service.getUrl())
        .hasQueryParameter("shipmentId", shipmentId);
  }

}
