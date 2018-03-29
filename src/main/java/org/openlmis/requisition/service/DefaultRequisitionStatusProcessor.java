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

import java.util.UUID;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("PMD.AvoidThrowingRawExceptionTypes")
public class DefaultRequisitionStatusProcessor implements RequisitionStatusProcessor {

  @Autowired
  private ConvertToOrderNotifier convertToOrderNotifier;

  @Autowired
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Autowired
  private ApprovalNotifier approvalNotifier;

  @Autowired
  private ApprovedRequisitionNotifier approvedRequisitionNotifier;

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  /**
   * Process requisition status change.
   * @param requisition a requisition that has just changed its status
   */
  @Override
  public void statusChange(Requisition requisition) {
    if (requisition.getStatus() == RequisitionStatus.RELEASED) {
      convertToOrderNotifier.notifyConvertToOrder(requisition);
    } else if (!requisition.isPreAuthorize()) {
      requisitionStatusNotifier.notifyStatusChanged(requisition);
    }

    if (requisition.isApprovable()) {
      assignInitialSupervisoryNode(requisition);
      approvalNotifier.notifyApprovers(requisition);
    }

    if (requisition.getStatus() == RequisitionStatus.APPROVED) {
      approvedRequisitionNotifier.notifyClerks(requisition);
    }
  }

  private void assignInitialSupervisoryNode(Requisition requisition) {
    if (requisition.getSupervisoryNodeId() == null) {
      UUID supervisoryNode = supervisoryNodeReferenceDataService.findSupervisoryNode(
          requisition.getProgramId(), requisition.getFacilityId()).getId();
      requisition.setSupervisoryNodeId(supervisoryNode);
    }
  }

}
