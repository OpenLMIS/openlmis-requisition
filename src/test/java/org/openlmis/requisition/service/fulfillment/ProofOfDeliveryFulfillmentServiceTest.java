package org.openlmis.requisition.service.fulfillment;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.service.BaseCommunicationServiceTest;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.URI;
import java.util.Collection;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class ProofOfDeliveryFulfillmentServiceTest extends BaseCommunicationServiceTest {

  @Test
  public void shouldFindByExternalId() throws Exception {
    // given
    ProofOfDeliveryDto pod = new ProofOfDeliveryDto();
    pod.setId(UUID.randomUUID());
    ResponseEntity<ProofOfDeliveryDto[]> response = mock(ResponseEntity.class);
    UUID externalId = UUID.randomUUID();

    // when
    when(response.getBody()).thenReturn(new ProofOfDeliveryDto[]{pod});
    when(restTemplate.getForEntity(any(URI.class), eq(ProofOfDeliveryDto[].class)))
        .thenReturn(response);

    ProofOfDeliveryFulfillmentService service = prepareService();
    Collection<ProofOfDeliveryDto> found = service.findByExternalId(externalId);

    // then
    verify(restTemplate).getForEntity(uriCaptor.capture(), eq(ProofOfDeliveryDto[].class));

    URI uri = uriCaptor.getValue();
    String url = String.format(
        "http://localhost/api/proofOfDeliveries/search?%s&externalId=%s",
        ACCESS_TOKEN, externalId
    );

    assertThat(uri.toString(), is(equalTo(url)));
    assertThat(found, hasSize(1));
    assertThat(found.iterator().next().getId(), is(equalTo(pod.getId())));
  }

  @Test
  public void shouldWorkWithEmptyExternalId() throws Exception {
    // given
    ResponseEntity<ProofOfDeliveryDto[]> response = mock(ResponseEntity.class);

    // when
    when(response.getBody()).thenReturn(new ProofOfDeliveryDto[0]);
    when(restTemplate.getForEntity(any(URI.class), eq(ProofOfDeliveryDto[].class)))
        .thenReturn(response);

    ProofOfDeliveryFulfillmentService service = prepareService();
    Collection<ProofOfDeliveryDto> found = service.findByExternalId(null);

    // then
    verify(restTemplate).getForEntity(uriCaptor.capture(), eq(ProofOfDeliveryDto[].class));

    URI uri = uriCaptor.getValue();
    String url = String.format("http://localhost/api/proofOfDeliveries/search?%s", ACCESS_TOKEN);

    assertThat(uri.toString(), is(equalTo(url)));
    assertThat(found, hasSize(0));
  }

  @Override
  protected ProofOfDeliveryFulfillmentService getService() {
    return new ProofOfDeliveryFulfillmentService();
  }

  @Override
  protected ProofOfDeliveryFulfillmentService prepareService() {
    BaseCommunicationService service = super.prepareService();

    ReflectionTestUtils.setField(service, "fulfillmentUrl", "http://localhost");

    return (ProofOfDeliveryFulfillmentService) service;
  }

}
