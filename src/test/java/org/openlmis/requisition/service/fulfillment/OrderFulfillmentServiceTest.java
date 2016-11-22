package org.openlmis.requisition.service.fulfillment;

import static org.mockito.Mockito.when;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.requisition.dto.OrderDto;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;


import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;


@RunWith(MockitoJUnitRunner.class)
public class OrderFulfillmentServiceTest {

  @InjectMocks
  private OrderFulfillmentService orderFulfillmentService;

  @Mock
  private RestTemplate restTemplate;

  @Before
  public void setUp() {
    Map<String, Object> params = new HashMap<>();
    params.put("grant_type", "client_credentials");

    Map<String, String> responseMap = new HashMap<>();
    responseMap.put("access_token", getToken());

    ResponseEntity<Object> response = new ResponseEntity<>(responseMap, HttpStatus.OK);

    when(restTemplate.exchange(Matchers.eq(buildUri("https://localhost/auth/oauth/token",
        params)), Matchers.eq(HttpMethod.POST), any(), Matchers.eq(Object.class)))
        .thenReturn(response);

    orderFulfillmentService = new OrderFulfillmentService(restTemplate);
    ReflectionTestUtils.setField(orderFulfillmentService, "authorizationUrl",
        "https://localhost/auth/oauth/token");
    ReflectionTestUtils.setField(orderFulfillmentService, "fulfillmentUrl",
        "https://localhost/fulfillment");
  }

  @Test
  public void shouldUseProperUrl() throws Exception {
    OrderDto orderDto = generate();
    orderFulfillmentService.create(orderDto);

    Map<String, String> paramsCreate = new HashMap<>();
    paramsCreate.put("access_token", getToken());

    verify(restTemplate, times(1)).postForObject(Matchers.eq(buildUri(
        "https://localhost/fulfillment/api/orders/", paramsCreate)), any(),
        Matchers.eq(Order.class));
  }

  private OrderDto generate() {
    OrderDto order = new OrderDto();
    order.setCreatedById(UUID.randomUUID());
    order.setProgramId(UUID.randomUUID());
    order.setRequestingFacilityId(UUID.randomUUID());
    order.setReceivingFacilityId(UUID.randomUUID());
    order.setSupplyingFacilityId(UUID.randomUUID());
    order.setOrderLineItems(new ArrayList());
    return order;
  }

  private String getToken() {
    return "418c89c5-7f21-4cd1-a63a-38c47892b0fe";
  }

  private URI buildUri(String url, Map<String, ?> params) {
    UriComponentsBuilder builder = UriComponentsBuilder.newInstance().uri(URI.create(url));

    params.entrySet().forEach(e -> builder.queryParam(e.getKey(), e.getValue()));

    return builder.build(true).toUri();
  }

}
