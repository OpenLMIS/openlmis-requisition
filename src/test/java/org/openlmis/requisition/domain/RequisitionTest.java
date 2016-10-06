package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;

import java.util.ArrayList;
import java.util.Collections;

public class RequisitionTest {

  private Requisition requisition;

  @Before
  public void setUp() {
    requisition = new Requisition();
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setRequisitionLineItems(new ArrayList<>());
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

    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();
    requisition.updateFrom(requisition, requisitionTemplate);

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
    verify(requisitionLineItem).calculateStockOnHand();
  }
}
