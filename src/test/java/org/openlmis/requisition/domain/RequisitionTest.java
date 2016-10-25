package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;

public class RequisitionTest {

  private Requisition requisition;
  private RequisitionLineItem requisitionLineItem;

  private UUID productId = UUID.randomUUID();

  @Before
  public void setUp() {
    requisition = new Requisition();
    requisitionLineItem = new RequisitionLineItem();

    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setRequestedQuantity(10);
    requisitionLineItem.setStockOnHand(20);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(productId);

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));
  }

  @Test
  public void shouldAuthorizeRequisitionIfItStatusIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingRequisitionWithNotSubmittedStatus()
      throws RequisitionException {
    requisition.authorize();
  }

  @Test
  public void shouldCalculateStockOnHandForRequisitionLineItemsWhenAuthorizing()
          throws RequisitionException, RequisitionTemplateColumnException {
    RequisitionLineItem requisitionLineItem = mock(RequisitionLineItem.class);
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);

    when(requisitionTemplate.isColumnCalculated("stockOnHand")).thenReturn(true);

    requisition.setRequisitionLineItems(new ArrayList<>(
            Collections.singletonList(requisitionLineItem)));

    Requisition newRequisition = new Requisition();

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();
    requisition.updateFrom(newRequisition, requisitionTemplate, Lists.newArrayList());

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
    verify(requisitionLineItem).calculateStockOnHand();
  }

  @Test
  public void shouldFindRequisitionLineItemByProductId() {
    RequisitionLineItem found = requisition.findLineByProductId(productId);

    assertNotNull(found);
    assertEquals(requisitionLineItem, found);
  }

  @Test
  public void shouldSetRequisitionFieldForLineItemsAfterUpdate() throws Exception {
    // given
    Requisition newRequisition = new Requisition();

    // existing line
    RequisitionLineItem firstRequisitionLineItem = new RequisitionLineItem();
    firstRequisitionLineItem.setId(requisitionLineItem.getId());
    firstRequisitionLineItem.setRequestedQuantity(10);
    firstRequisitionLineItem.setStockOnHand(20);
    firstRequisitionLineItem.setRequisition(newRequisition);
    firstRequisitionLineItem.setOrderableProductId(productId);

    // new line
    RequisitionLineItem secondRequisitionLineItem = new RequisitionLineItem();
    secondRequisitionLineItem.setRequestedQuantity(10);
    secondRequisitionLineItem.setStockOnHand(20);
    secondRequisitionLineItem.setRequisition(newRequisition);
    secondRequisitionLineItem.setOrderableProductId(productId);

    newRequisition.setId(UUID.randomUUID());
    newRequisition.setStatus(RequisitionStatus.INITIATED);
    newRequisition.setRequisitionLineItems(
        Lists.newArrayList(firstRequisitionLineItem, secondRequisitionLineItem)
    );

    // when
    requisition.setId(UUID.randomUUID());
    requisition.updateFrom(newRequisition, mock(RequisitionTemplate.class), Lists.newArrayList());

    // then
    requisition
        .getRequisitionLineItems()
        .forEach(line -> assertThat(line.getRequisition().getId(), is(requisition.getId())));
  }
}
