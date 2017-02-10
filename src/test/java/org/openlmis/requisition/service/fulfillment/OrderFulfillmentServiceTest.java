package org.openlmis.requisition.service.fulfillment;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;

import java.net.URI;
import java.util.List;
import java.util.UUID;

public class OrderFulfillmentServiceTest extends BaseFulfillmentServiceTest<OrderDto> {

  @Captor
  protected ArgumentCaptor<HttpEntity> entityCaptor;

  @Override
  protected BaseCommunicationService<OrderDto> getService() {
    return new OrderFulfillmentService();
  }

  @Override
  protected OrderDto generateInstance() {
    OrderDto order = new OrderDto();
    order.setId(UUID.randomUUID());
    
    return order;
  }

  @Test
  public void shouldSendRequestToCreateOrder() throws Exception {
    // given
    OrderFulfillmentService service = (OrderFulfillmentService) prepareService();
    OrderDto order = generateInstance();

    // then
    service.create(order);

    // then
    verify(restTemplate)
        .postForEntity(uriCaptor.capture(), entityCaptor.capture(), eq(Object.class));

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl() + "?" + ACCESS_TOKEN;

    assertThat(uri.toString(), is(equalTo(url)));

    HttpEntity entity = entityCaptor.getValue();
    Object body = entity.getBody();

    assertThat(body, instanceOf(OrderDto.class));
    assertThat(((OrderDto) body).getId(), is(equalTo(order.getId())));
  }

  @Test
  public void shouldGetProofOfDeliveries() throws Exception {
    // given
    OrderFulfillmentService service = (OrderFulfillmentService) prepareService();
    OrderDto order = generateInstance();
    ProofOfDeliveryDto pod = new ProofOfDeliveryDto();
    pod.setId(UUID.randomUUID());

    ResponseEntity<ProofOfDeliveryDto[]> response = mock(ResponseEntity.class);
    when(restTemplate.getForEntity(any(URI.class), eq(ProofOfDeliveryDto[].class)))
        .thenReturn(response);
    when(response.getBody()).thenReturn(new ProofOfDeliveryDto[]{pod});

    // when
    List<ProofOfDeliveryDto> list = service.getProofOfDeliveries(order.getId());

    // then
    verify(restTemplate).getForEntity(
        uriCaptor.capture(), eq(ProofOfDeliveryDto[].class)
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl() + order.getId()
        + "/proofOfDeliveries?" + ACCESS_TOKEN;

    assertThat(uri.toString(), is(equalTo(url)));
    
    assertThat(list, hasSize(1));
    assertThat(list.get(0).getId(), is(equalTo(pod.getId())));
  }
}
