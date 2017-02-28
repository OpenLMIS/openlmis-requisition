/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.service;

import org.javers.core.Javers;
import org.javers.core.diff.Change;
import org.javers.repository.jql.QueryBuilder;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@SuppressWarnings("PMD.AvoidThrowingRawExceptionTypes")
public class DefaultRequisitionStatusProcessor implements RequisitionStatusProcessor {

  @Autowired
  private Javers javers;

  @Autowired
  private ConvertToOrderNotifier convertToOrderNotifier;

  @Autowired
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Autowired
  private ApprovalNotifier approvalNotifier;

  /**
   * Process requisition status change.
   * @param requisition a requisition that has just changed its status
   */
  public void statusChange(Requisition requisition) {
    Change lastChange = findLastChange(requisition);
    if (lastChange != null) {
      if (requisition.getStatus() == RequisitionStatus.RELEASED) {
        convertToOrderNotifier.notifyConvertToOrder(requisition);
      } else {
        requisitionStatusNotifier.notifyStatusChanged(requisition, lastChange);
      }
      if (requisition.isApprovable()) {
        approvalNotifier.notifyApprovers(requisition);
      }
    } else {
      throw new RuntimeException("Requisition's status change not found. "
          + "Make sure that it was saved before calling RequisitionStatusProcessor.statusChange()");
    }
  }

  private Change findLastChange(Requisition requisition) {
    List<Change> statusChanges = javers.findChanges(QueryBuilder
        .byInstance(requisition)
        .andProperty(Requisition.STATUS)
        .build()
    );
    List<Change> nodeChanges = javers.findChanges(QueryBuilder
        .byInstance(requisition)
        .andProperty(Requisition.SUPERVISORY_NODE_ID)
        .build()
    );
    Change lastStatusChange = (statusChanges.isEmpty()) ? null :
        statusChanges.get(0);
    Change lastNodeChange = (nodeChanges.isEmpty()) ? null :
        nodeChanges.get(0);
    // If node change was after status change, return it
    if (lastStatusChange != null && lastNodeChange != null
        && lastNodeChange.getCommitMetadata().get().getCommitDate().isAfter(
            lastStatusChange.getCommitMetadata().get().getCommitDate())) {
      return lastNodeChange;
    }
    // otherwise return last status change
    return lastStatusChange;
  }
}
