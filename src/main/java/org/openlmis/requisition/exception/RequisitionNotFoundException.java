package org.openlmis.requisition.exception;

import java.util.UUID;

public class RequisitionNotFoundException extends RequisitionException {

  public RequisitionNotFoundException(UUID requisitionId) {
    super("Requisition not found for ID: " + requisitionId);
  }
}
