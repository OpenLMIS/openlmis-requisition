package org.openlmis.requisition.service;

import org.javers.core.diff.Change;
import org.openlmis.requisition.domain.Requisition;

public interface RequisitionStatusNotifier {

  /**
   * Notify user(s) that the requisition's status has changed.
   *
   * @param requisition a requisition that has just changed its status
   * @param change Javers change containing requisition's status, the author, the time, etc.
   */
  void notifyStatusChanged(Requisition requisition, Change change);
}