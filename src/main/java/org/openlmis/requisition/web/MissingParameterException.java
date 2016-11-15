package org.openlmis.requisition.web;

import org.openlmis.requisition.exception.RequisitionException;

public class MissingParameterException extends RequisitionException {

  public MissingParameterException(String message) {
    super(message);
  }
}
