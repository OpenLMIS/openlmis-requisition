package org.openlmis.requisition.dto;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import com.google.common.collect.Lists;

import org.junit.Test;

import java.util.UUID;

public class ProofOfDeliveryDtoTest {

  @Test
  public void shouldbeSubmittedIfOrderHasCorrectStatus() throws Exception {
    OrderDto order = OrderDto.builder().status(OrderStatus.RECEIVED).build();
    ProofOfDeliveryDto pod = ProofOfDeliveryDto.builder().order(order).build();

    assertThat(pod.isSubmitted(), is(true));
  }

  @Test
  public void shouldbeNotSubmittedIfOrderHasIncorrectStatus() throws Exception {
    for (OrderStatus status : OrderStatus.values()) {
      if (OrderStatus.RECEIVED == status) {
        // for this status method should return true
        // we test this above
        continue;
      }

      OrderDto order = OrderDto.builder().status(status).build();
      ProofOfDeliveryDto pod = ProofOfDeliveryDto.builder().order(order).build();

      assertThat("POD should not be submitted for status: " + status, pod.isSubmitted(), is(false));
    }
  }

  @Test
  public void shouldFindLineByProductId() throws Exception {
    UUID productId1 = UUID.randomUUID();
    UUID productId2 = UUID.randomUUID();

    ProofOfDeliveryLineItemDto line1 = ProofOfDeliveryLineItemDto
        .builder()
        .id(UUID.randomUUID())
        .orderLineItem(OrderLineItemDto
            .builder()
            .orderable(OrderableDto
                .builder()
                .id(productId1)
                .build())
            .build())
        .build();
    ProofOfDeliveryLineItemDto line2 = ProofOfDeliveryLineItemDto
        .builder()
        .id(UUID.randomUUID())
        .orderLineItem(OrderLineItemDto
            .builder()
            .orderable(OrderableDto
                .builder()
                .id(productId2)
                .build())
            .build())
        .build();

    ProofOfDeliveryDto pod = ProofOfDeliveryDto
        .builder()
        .proofOfDeliveryLineItems(Lists.newArrayList(line1, line2))
        .build();

    assertThat(pod.findLineByProductId(UUID.randomUUID()), is(nullValue()));
    assertThat(pod.findLineByProductId(productId1).getId(), is(equalTo(line1.getId())));
    assertThat(pod.findLineByProductId(productId2).getId(), is(equalTo(line2.getId())));
  }
}
