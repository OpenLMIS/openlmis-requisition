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
 * Renders one of two values depending on a flag, for example {@code {emergency:E|R}}. The
 * argument is required and is split on the first {@code |}. A {@code null} flag is treated as
 * {@code false}.
 *
 * @param <T> type of entity the code is generated for
 */
public final class ConditionalToken<T> implements PatternToken<T> {

  private static final String SEPARATOR = "|";

  private final String description;
  private final Function<T, Boolean> flagOf;

  public ConditionalToken(String description, Function<T, Boolean> flagOf) {
    this.description = description;
    this.flagOf = flagOf;
  }

  @Override
  public Function<T, String> bind(String argument) {
    int separator = argument == null ? -1 : argument.indexOf(SEPARATOR);
    if (separator < 0) {
      throw new CodeGenerationException("Token for " + description
          + " requires an argument in the form valueIfTrue|valueIfFalse");
    }
    String whenTrue = argument.substring(0, separator);
    String whenFalse = argument.substring(separator + 1);
    return entity -> Boolean.TRUE.equals(flagOf.apply(entity)) ? whenTrue : whenFalse;
  }

  @Override
  public String apply(T entity, String argument) {
    return bind(argument).apply(entity);
  }
}
