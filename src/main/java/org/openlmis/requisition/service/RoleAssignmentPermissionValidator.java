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

import java.util.List;
import java.util.Objects;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.RoleReferenceDataService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class RoleAssignmentPermissionValidator extends BasePermissionValidator {

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private RoleReferenceDataService roleReferenceDataService;

  @Override
  boolean checkUserToken(PermissionValidationDetails details, Profiler profiler) {
    profiler.start("GET_CURRENT_USER");
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("GET_RIGHT");
    RightDto right = authenticationHelper.getRight(details.getRightName());

    profiler.start("GET_ROLES_FOR_RIGHT");
    List<RoleDto> roles = roleReferenceDataService.search(right.getId());

    profiler.start("CHECK_HAS_ROLE");
    for (int i = 0, length = roles.size(); i < length; ++i) {
      RoleDto role = roles.get(i);

      if (Objects.nonNull(details.getSupervisoryNodeId())
          && user.hasMatchingSupervisorySupervisionRole(role.getId(),
          details.getProgramId(), details.getSupervisoryNodeId())) {
        return true;
      }

      if (!details.containsPartnerRequisition()
          && user.hasMatchingHomeFacilitySupervisionRole(role.getId(),
          details.getProgramId(), details.getFacilityId())) {
        return true;
      }
    }

    return false;
  }

}
