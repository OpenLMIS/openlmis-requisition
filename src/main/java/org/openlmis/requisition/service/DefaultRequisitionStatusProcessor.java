package org.openlmis.requisition.service;

import org.javers.core.Javers;
import org.javers.core.diff.Change;
import org.javers.repository.jql.QueryBuilder;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.JaversChangeNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class DefaultRequisitionStatusProcessor implements RequisitionStatusProcessor {

  @Autowired
  private Javers javers;

  @Autowired
  private ConvertToOrderNotifier convertToOrderNotifier;

  /**
   * Process requisition status change.
   * @param requisition a requisition that has just changed its status
   */
  public void statusChange(Requisition requisition) throws JaversChangeNotFoundException {
    List<Change> statusChanges = javers.findChanges(QueryBuilder
        .byInstance(requisition)
        .andProperty(Requisition.STATUS)
        .build()
    );
    if (!statusChanges.isEmpty()) {
      Change lastChange = statusChanges.get(statusChanges.size() - 1);
      if (requisition.getStatus() == RequisitionStatus.RELEASED) {
        convertToOrderNotifier.notifyStatusChanged(requisition, lastChange);
      }
    } else {
      throw new JaversChangeNotFoundException("Requisition's status change not found. "
          + "Make sure that it was saved before calling RequisitionStatusProcessor.statusChange()");
    }
  }
}
