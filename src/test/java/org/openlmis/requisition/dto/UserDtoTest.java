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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class UserDtoTest {

  private UserDto userDto = new UserDto();

  @Test
  public void shouldPrintNameAsFirstLastName() {
    userDto.setFirstName("John");
    userDto.setLastName("Doe");

    assertEquals("John Doe", userDto.printName());
  }

  @Test
  public void shouldPrintNameAsOnlyFirstName() {
    userDto.setFirstName("John");

    assertEquals("John", userDto.printName());
  }

  @Test
  public void shouldPrintNameAsOnlyLastName() {
    userDto.setLastName("Doe");

    assertEquals("Doe", userDto.printName());
  }

  @Test
  public void shouldPrintNameAsUsername() {
    userDto.setUsername("jdoe");

    assertEquals("jdoe", userDto.printName());
  }
}
