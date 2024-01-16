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

package org.openlmis.requisition.repository.custom;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.apache.commons.lang3.tuple.Pair;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionPeriod;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface RequisitionRepositoryCustom {

  Page<Requisition> searchRequisitions(RequisitionSearchParams params,
      List<String> userPermissionStrings, Set<Pair<UUID, UUID>> programNodePairs,
      Pageable pageable);

  List<Requisition> searchRequisitions(UUID processingPeriod,
      UUID facility, UUID program, Boolean emergency);

  Optional<Requisition> findRegularRequisition(UUID processingPeriod, UUID facility, UUID program);

  List<RequisitionPeriod> searchRequisitionIdAndStatusPairs(UUID facility, UUID program,
      Boolean emergency);

  Page<Requisition> searchApprovedRequisitions(UUID facilityId,
      Set<Pair<UUID, UUID>> programSupervisoryNodePair, Pageable pageable);

  Page<Requisition> searchApprovableRequisitionsByProgramSupervisoryNodePairs(
      Set<Pair<UUID, UUID>> programNodePairs, Pageable pageable);

  Long countApprovableRequisitionsByProgramSupervisoryNodePairs(
      Set<Pair<UUID, UUID>> programNodePairs);
}
