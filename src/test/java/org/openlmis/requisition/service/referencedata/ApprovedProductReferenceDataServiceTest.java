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

package org.openlmis.requisition.service.referencedata;

import static java.util.UUID.randomUUID;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import java.net.URI;
import java.util.List;
import java.util.UUID;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.service.BaseCommunicationServiceTest;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.utils.DynamicPageTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.test.util.ReflectionTestUtils;

public class ApprovedProductReferenceDataServiceTest
    extends BaseCommunicationServiceTest<ApprovedProductDto> {

  private ApprovedProductReferenceDataService service;

  @Override
  protected ApprovedProductDto generateInstance() {
    return new ApprovedProductDto();
  }

  @Override
  protected BaseCommunicationService<ApprovedProductDto> getService() {
    return new ApprovedProductReferenceDataService();
  }

  @Override
  @Before
  public void setUp() {
    super.setUp();

    service = (ApprovedProductReferenceDataService) prepareService();
    ReflectionTestUtils.setField(service, "referenceDataUrl", "http://localhost");
  }

  @Test
  public void shouldReturnApprovedProducts() {
    ProgramDto program = new ProgramDtoDataBuilder().build();
    UUID facilityId = randomUUID();

    ApprovedProductDto product = new ApprovedProductDtoDataBuilder()
        .withOrderable(new OrderableDtoDataBuilder()
            .withProgramOrderable(program.getId(), true)
            .build())
        .withProgram(program)
        .build();

    mockPageResponseEntity(product);

    ApproveProducts response = service.getApprovedProducts(facilityId, program.getId());

    assertThat(response.getOrderableIds(), hasSize(1));
    assertThat(response.getOrderableIds(), hasItem(product.getOrderable().getId()));

    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET), entityCaptor.capture(),
        any(DynamicPageTypeReference.class)
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl()
        + facilityId + "/approvedProducts";

    assertThat(uri.toString(), startsWith(url));

    List<NameValuePair> parse = URLEncodedUtils.parse(uri, "UTF-8");

    assertQueryParameter(parse, "programId", program.getId());

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }
}
