package org.openlmis.requisition.exception;

import java.util.UUID;

public class CommentNotFoundException extends RequisitionException {

  public CommentNotFoundException(UUID requisitionId) {
    super("Requisition comment not found for ID: " + requisitionId);
  }
}
