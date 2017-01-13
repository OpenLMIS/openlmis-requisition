package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

/**
 * Signals user being unauthorized in external api.
 */
public class AuthenticationMessageException extends BaseMessageException {

  public AuthenticationMessageException(Message message) {
    super(message);
  }
}
