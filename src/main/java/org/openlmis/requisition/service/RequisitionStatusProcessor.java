package org.openlmis.requisition.service;

import org.javers.common.exception.JaversException;
import org.openlmis.requisition.domain.Requisition;

public interface RequisitionStatusProcessor {

  /**
   * Process requisition status change.
   *
   * @param requisition a requisition that has just changed its status
   */
  void statusChange(Requisition requisition) throws JaversException;
}
