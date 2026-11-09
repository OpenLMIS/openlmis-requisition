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
 * Source of sequence values used by the {@code {seq}} token.
 */
public interface CodeSequenceRepository {

  /**
   * Atomically claims the next value of the counter identified by the given key. The first call
   * for a key returns 1. Implementations must hold a lock on the counter until the surrounding
   * transaction ends, so a rolled back entity does not consume a value.
   *
   * @param scopeKey canonical key of the counter
   * @return next value, starting from 1
   */
  int next(String scopeKey);
}
