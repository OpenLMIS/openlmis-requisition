package org.openlmis.requisition.web;


import org.openlmis.requisition.exception.BaseMessageException;
import org.openlmis.utils.Message;

public class PermissionMessageException extends BaseMessageException {

  public PermissionMessageException(Message message) {
    super(message);
  }

}
