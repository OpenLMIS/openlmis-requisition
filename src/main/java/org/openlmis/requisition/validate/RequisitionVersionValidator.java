package org.openlmis.requisition.validate;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DATE_MODIFIED_MISMATCH;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.exception.VersionMismatchException;
import org.springframework.stereotype.Component;

import java.time.ZonedDateTime;

/**
 * Validates the date modified of a requisition against an incoming object.
 * If the date modified in the incoming object is present and does not match
 * the current one, the validation should lead to a 409 error. If the timestamp
 * is not part of the incoming object, we can skip it.
 */
@Component
public class RequisitionVersionValidator {

  /**
   * Validates if the date modified of the incoming requisition matches
   * the date modified of the existing requisition. If not, a
   * {@link VersionMismatchException} is thrown. If the incoming requisition has
   * no modified date, that will not trigger the exception.
   * @param incomingReq the requisition to validate
   * @param existingReq the existing version of the requisition
   */
  public void validateRequisitionTimestamps(Requisition incomingReq,
                                            Requisition existingReq) {
    ZonedDateTime dateModified = incomingReq.getModifiedDate();
    if (dateModified != null
        && !dateModified.isEqual(existingReq.getModifiedDate())) {
      throw new VersionMismatchException(ERROR_DATE_MODIFIED_MISMATCH);
    }
  }
}
