package org.openlmis.requisition.validate;

import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.exception.VersionMismatchException;

import java.time.ZonedDateTime;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionVersionValidatorTest {

  private RequisitionVersionValidator requisitionVersionValidator
      = new RequisitionVersionValidator();

  @Mock
  private Requisition incomingReq;

  @Mock
  private Requisition existingReq;

  @Test
  public void shouldAcceptCorrectModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();

    testModifiedDateValidation(dateModified, ZonedDateTime.from(dateModified));
  }

  @Test
  public void shouldAcceptWithNoModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();

    testModifiedDateValidation(null, dateModified);
  }

  @Test(expected = VersionMismatchException.class)
  public void shouldRejectFutureModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();
    ZonedDateTime incomingDate = dateModified.plusYears(1);

    testModifiedDateValidation(incomingDate, dateModified);
  }

  @Test(expected = VersionMismatchException.class)
  public void shouldRejectPastModifiedDate() {
    ZonedDateTime dateModified = ZonedDateTime.now();
    ZonedDateTime incomingDate = dateModified.minusMinutes(5);

    testModifiedDateValidation(incomingDate, dateModified);
  }

  private void testModifiedDateValidation(ZonedDateTime incomingDate, ZonedDateTime
      databaseDate) {
    when(incomingReq.getModifiedDate()).thenReturn(incomingDate);
    when(existingReq.getModifiedDate()).thenReturn(databaseDate);
    requisitionVersionValidator.validateRequisitionTimestamps(
        incomingReq, existingReq);
  }
}
