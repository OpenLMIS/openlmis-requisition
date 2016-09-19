package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.exception.RequisitionException;

import java.util.ArrayList;
import java.util.Collections;

public class RequisitionTest {

  private Requisition requisition;

  @Before
  public void setUp() {
    requisition = new Requisition();
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setRequisitionLines(new ArrayList<>());
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
  public void shouldCalculateStockOnHandForRequisitionLinesWhenAuthorizing()
      throws RequisitionException {
    RequisitionLine requisitionLine = mock(RequisitionLine.class);

    requisition.setRequisitionLines(Collections.singletonList(requisitionLine));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
    verify(requisitionLine).calculateStockOnHand();
  }
}
