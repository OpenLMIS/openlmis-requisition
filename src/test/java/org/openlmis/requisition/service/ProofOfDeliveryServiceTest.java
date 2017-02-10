package org.openlmis.requisition.service;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;

import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class ProofOfDeliveryServiceTest {

  @Mock
  private PeriodService periodService;

  @Mock
  private OrderFulfillmentService orderFulfillmentService;

  @InjectMocks
  private ProofOfDeliveryService proofOfDeliveryService;

  @Mock
  private Requisition requisition;

  @Mock
  private OrderDto order;

  @Mock
  private ProofOfDeliveryDto proofOfDelivery;

  @Before
  public void setUp() throws Exception {
    when(requisition.getFacilityId()).thenReturn(UUID.randomUUID());
    when(requisition.getProgramId()).thenReturn(UUID.randomUUID());
    when(requisition.getProcessingPeriodId()).thenReturn(UUID.randomUUID());

    when(order.getId()).thenReturn(UUID.randomUUID());
    when(proofOfDelivery.getId()).thenReturn(UUID.randomUUID());
  }

  @Test
  public void shouldReturnNullIfRequisitionWasSkipped() throws Exception {
    when(requisition.getStatus()).thenReturn(RequisitionStatus.SKIPPED);
    assertThat(proofOfDeliveryService.get(requisition), is(nullValue()));
  }

  @Test
  public void shouldReturnNullIfRequisitionHasEmergencyFlag() throws Exception {
    when(requisition.getEmergency()).thenReturn(true);
    assertThat(proofOfDeliveryService.get(requisition), is(nullValue()));
  }

  @Test
  public void shouldReturnNullIfThereIsNoOrders() throws Exception {
    when(orderFulfillmentService.search(
        null, requisition.getFacilityId(), requisition.getProgramId(),
        requisition.getProcessingPeriodId(), null
    )).thenReturn(null);

    assertThat(proofOfDeliveryService.get(requisition), is(nullValue()));
  }

  @Test
  public void shouldReturnNullIfThereIsNoPods() throws Exception {
    when(orderFulfillmentService.search(
        null, requisition.getFacilityId(), requisition.getProgramId(),
        requisition.getProcessingPeriodId(), null
    )).thenReturn(Lists.newArrayList(order));

    when(orderFulfillmentService.getProofOfDeliveries(order.getId()))
        .thenReturn(null);

    assertThat(proofOfDeliveryService.get(requisition), is(nullValue()));
  }

  @Test
  public void shouldReturnRelatedPod() throws Exception {
    when(orderFulfillmentService.search(
        null, requisition.getFacilityId(), requisition.getProgramId(),
        requisition.getProcessingPeriodId(), null
    )).thenReturn(Lists.newArrayList(order));

    when(orderFulfillmentService.getProofOfDeliveries(order.getId()))
        .thenReturn(Lists.newArrayList(proofOfDelivery));

    assertThat(proofOfDeliveryService.get(requisition), is(proofOfDelivery));
  }
}
