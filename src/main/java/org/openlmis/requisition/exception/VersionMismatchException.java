package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

/**
 * Signals a version mismatch between objects that should lead to
 * a 409 conflict error.
 */
public class VersionMismatchException extends BaseMessageException {

  public VersionMismatchException(Message message) {
    super(message);
  }

  public VersionMismatchException(String messageKey) {
    super(messageKey);
  }
}
