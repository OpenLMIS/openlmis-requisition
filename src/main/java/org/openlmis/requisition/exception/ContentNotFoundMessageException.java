package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

/**
 * Exception thrown when resource was not found.
 */
public class ContentNotFoundMessageException extends BaseMessageException {
  public ContentNotFoundMessageException(Message message) {
    super(message);
  }
}
