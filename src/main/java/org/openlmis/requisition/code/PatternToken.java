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

import java.util.function.Function;

/**
 * Single placeholder of a code pattern, for example {@code {facilityCode}} or
 * {@code {date:yyyyMMdd}}. Implementations must not depend on any domain class other than the
 * generic entity type, so this package can be copied between services unchanged.
 *
 * @param <T> type of entity the code is generated for
 */
@FunctionalInterface
public interface PatternToken<T> {

  /**
   * Renders the token for the given entity.
   *
   * @param entity entity the code is generated for
   * @param argument text after the colon in the pattern, or {@code null} when absent
   * @return rendered text, never {@code null}
   */
  String apply(T entity, String argument);

  /**
   * Called once when the pattern is compiled. Implementations with an argument that is expensive
   * to parse or may be invalid should override this method, validate the argument and return an
   * optimised renderer. Any {@link CodeGenerationException} thrown here stops the application from
   * starting.
   *
   * @param argument text after the colon in the pattern, or {@code null} when absent
   * @return renderer used for every generated code
   */
  default Function<T, String> bind(String argument) {
    return entity -> apply(entity, argument);
  }
}
