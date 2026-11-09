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

package org.openlmis.requisition.code;

/**
 * Generates a human-readable code for an entity. Callers depend on this interface rather than on
 * a concrete generator, so they can be tested with a simple mock.
 *
 * @param <T> type of entity the code is generated for
 */
@FunctionalInterface
public interface CodeGenerator<T> {

  /**
   * Generates a new code for the given entity. Must be called inside the transaction that saves
   * the entity, so a claimed sequence value is released when the transaction rolls back.
   */
  String generate(T entity);
}
