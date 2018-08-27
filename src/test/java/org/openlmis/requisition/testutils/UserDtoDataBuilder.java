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

package org.openlmis.requisition.testutils;

import java.util.Collections;
import java.util.Set;
import java.util.UUID;
import org.apache.commons.lang3.RandomStringUtils;
import org.openlmis.requisition.dto.RoleAssignmentDto;
import org.openlmis.requisition.dto.UserDto;

public class UserDtoDataBuilder {
  private UUID id = UUID.randomUUID();
  private String username = RandomStringUtils.randomAlphanumeric(5);
  private String firstName = RandomStringUtils.randomAlphanumeric(5);
  private String lastName = RandomStringUtils.randomAlphanumeric(5);
  private String email = username + "@some.where";
  private boolean verified = true;
  private UUID homeFacilityId = UUID.randomUUID();
  private Set<RoleAssignmentDto> roleAssignments = Collections.emptySet();
  private Boolean allowNotify = true;
  private boolean active = true;

  public UserDtoDataBuilder withUsername(String username) {
    this.username = username;
    return this;
  }

  public UserDtoDataBuilder asInactive() {
    active = false;
    return this;
  }

  public UserDtoDataBuilder asUnverified() {
    verified = false;
    return this;
  }

  public UserDtoDataBuilder denyNotify() {
    allowNotify = false;
    return this;
  }

  public UserDtoDataBuilder withoutEmail() {
    email = null;
    return this;
  }

  /**
   * Creates new instance of {@link UserDto}.
   */
  public UserDto build() {
    UserDto user = new UserDto(
        username, firstName, lastName, email, verified, homeFacilityId, roleAssignments,
        allowNotify, active
    );
    user.setId(id);

    return user;
  }
}
