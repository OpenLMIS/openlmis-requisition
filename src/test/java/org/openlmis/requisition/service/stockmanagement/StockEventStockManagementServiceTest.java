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

package org.openlmis.requisition.service.stockmanagement;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.net.URI;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.utils.RequestHelper;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpServerErrorException;

@SuppressWarnings("PMD.TooManyMethods")
public class StockEventStockManagementServiceTest
    extends BaseStockmanagementServiceTest<StockEventDto> {

  private StockEventStockManagementService service;
  private StockEventDto stockEventDto;

  @Before
  public void setUp() {
    super.setUp();
    service = (StockEventStockManagementService) prepareService();
    stockEventDto = generateInstance();
  }

  @Test
  public void shouldSubmitStockEvent() {
    givenMockedOkResponse();

    whenEventIsSubmitted();

    thenPostRequestIsSent();
    andUriIsCorrect();
    andEntityIsCorrect();
  }

  @Test
  public void shouldResubmitEventWhenConflict() {
    givenMockedConflictResponse();

    whenEventIsSubmitted();

    thenPostRequestIsSentTwice();
    andUriIsCorrect();
    andEntityIsCorrect();
  }

  private void givenMockedOkResponse() {
    given(restTemplate.exchange(any(URI.class), any(HttpMethod.class), any(HttpEntity.class),
        any(Class.class))).willReturn(ResponseEntity.ok(UUID.randomUUID()));
  }

  private void givenMockedConflictResponse() {
    given(restTemplate.exchange(any(URI.class), any(HttpMethod.class), any(HttpEntity.class),
        any(Class.class)))
        .willThrow(new HttpServerErrorException(HttpStatus.valueOf(409)))
        .willReturn(ResponseEntity.ok(UUID.randomUUID()));
  }

  private void whenEventIsSubmitted() {
    service.submit(stockEventDto);
  }

  private void thenPostRequestIsSent() {
    verifyRequest(1);
  }

  private void thenPostRequestIsSentTwice() {
    verifyRequest(2);
  }

  private void andUriIsCorrect() {
    assertEquals(RequestHelper.createUri(serviceUrl + "/api/stockEvents"), uriCaptor.getValue());
  }

  private void andEntityIsCorrect() {
    assertNotNull(entityCaptor.getValue().getBody());
    assertEquals(stockEventDto, entityCaptor.getValue().getBody());
    assertAuthHeader(entityCaptor.getValue());
  }

  private void verifyRequest(int times) {
    verify(restTemplate, times(times))
        .exchange(uriCaptor.capture(), eq(HttpMethod.POST), entityCaptor.capture(), eq(UUID.class));
  }

  @Override
  protected StockEventDto generateInstance() {
    return DtoGenerator.of(StockEventDto.class);
  }

  @Override
  protected BaseCommunicationService<StockEventDto> getService() {
    return new StockEventStockManagementService();
  }
}