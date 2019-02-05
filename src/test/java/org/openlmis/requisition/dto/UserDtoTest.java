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

package org.openlmis.requisition.dto;

import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.Set;
import java.util.UUID;
import nl.jqno.equalsverifier.EqualsVerifier;
import nl.jqno.equalsverifier.Warning;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.UserDtoDataBuilder;

@SuppressWarnings("PMD.TooManyMethods")
public class UserDtoTest {

  private UserDto userDto;

  private RoleAssignmentDto homeFacilityRole;
  private RoleAssignmentDto supervisionRole;

  @Before
  public void setUp() {
    homeFacilityRole = new RoleAssignmentDto(
        UUID.randomUUID(), UUID.randomUUID(), null, null);
    supervisionRole = new RoleAssignmentDto(
        UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), null);
    RoleAssignmentDto anotherSupervisionRole = new RoleAssignmentDto(
        UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), null);

    userDto = new UserDtoDataBuilder()
        .withUsername("jdoe")
        .withoutFirstName()
        .withoutLastName()
        .withRoleAssignment(homeFacilityRole)
        .withRoleAssignment(supervisionRole)
        .withRoleAssignment(anotherSupervisionRole)
        .build();
  }

  @Test
  public void shouldPrintNameAsFirstLastName() {
    userDto.setFirstName("John");
    userDto.setLastName("Doe");

    assertThat(userDto.printName()).isEqualTo("John Doe");
  }

  @Test
  public void shouldPrintNameAsOnlyFirstName() {
    userDto.setFirstName("John");

    assertThat(userDto.printName()).isEqualTo("John");
  }

  @Test
  public void shouldPrintNameAsOnlyLastName() {
    userDto.setLastName("Doe");

    assertThat(userDto.printName()).isEqualTo("Doe");
  }

  @Test
  public void shouldPrintNameAsUsername() {
    assertThat(userDto.printName()).isEqualTo("jdoe");
  }

  @Test
  public void equalsContract() {
    RoleAssignmentDto roleAssignments = DtoGenerator.of(RoleAssignmentDto.class);

    EqualsVerifier
        .forClass(UserDto.class)
        .withPrefabValues(Set.class, emptySet(), singleton(roleAssignments))
        .withRedefinedSuperclass()
        .suppress(Warning.NONFINAL_FIELDS) // fields in DTO cannot be final
        .verify();
  }

  @Test
  public void shouldHaveSupervisionRole() {
    assertThat(userDto.hasSupervisorySupervisionRole(supervisionRole.getRoleId(),
        supervisionRole.getProgramId(), supervisionRole.getSupervisoryNodeId()))
        .isTrue();
  }

  @Test
  public void shouldNotHaveSupervisionRoleIfUserHasNoRole() {
    assertThat(userDto
        .hasSupervisorySupervisionRole(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID()))
        .isFalse();
  }

  @Test
  public void shouldHaveHomeFacilitySupervisionRole() {
    assertThat(userDto.hasHomeFacilitySupervisionRole(homeFacilityRole.getRoleId(),
        homeFacilityRole.getProgramId(), userDto.getHomeFacilityId()))
        .isTrue();
  }

  @Test
  public void shouldNotHaveHomeFacilitySupervisionRoleIfUserHasNoHomeFacility() {
    userDto.setHomeFacilityId(null);

    assertThat(userDto.hasHomeFacilitySupervisionRole(homeFacilityRole.getRoleId(),
        homeFacilityRole.getProgramId(), UUID.randomUUID()))
        .isFalse();
  }

  @Test
  public void shouldNotHaveHomeFacilitySupervisionRoleIfHomeFacilityNotMatch() {
    assertThat(userDto.hasHomeFacilitySupervisionRole(homeFacilityRole.getRoleId(),
        homeFacilityRole.getProgramId(), UUID.randomUUID()))
        .isFalse();
  }

  @Test
  public void shouldNotHaveHomeFacilitySupervisionRoleIfUserHasNoRole() {
    assertThat(userDto.hasHomeFacilitySupervisionRole(UUID.randomUUID(),
        UUID.randomUUID(), UUID.randomUUID()))
        .isFalse();
  }
}
