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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

/**
 * Renders a date using a {@link DateTimeFormatter} pattern given as the argument, for example
 * {@code {date:ddMMyyyy}} or {@code {date:yyyy-MM-dd}}. Without an argument it renders
 * {@value #DEFAULT_FORMAT}.
 *
 * @param <T> type of entity the code is generated for
 */
public final class DateToken<T> implements PatternToken<T> {

  static final String DEFAULT_FORMAT = "yyyyMMdd";

  private final String description;
  private final Function<T, LocalDate> dateOf;

  public DateToken(String description, Function<T, LocalDate> dateOf) {
    this.description = description;
    this.dateOf = dateOf;
  }

  @Override
  public Function<T, String> bind(String argument) {
    String format = argument == null || argument.isEmpty() ? DEFAULT_FORMAT : argument;
    DateTimeFormatter formatter;
    try {
      formatter = DateTimeFormatter.ofPattern(format);
      formatter.format(LocalDate.of(2000, 1, 1));
    } catch (IllegalArgumentException | java.time.DateTimeException ex) {
      throw new CodeGenerationException(
          "Invalid date format '" + format + "' for " + description, ex);
    }
    return entity -> render(entity, formatter);
  }

  @Override
  public String apply(T entity, String argument) {
    return bind(argument).apply(entity);
  }

  private String render(T entity, DateTimeFormatter formatter) {
    LocalDate date = dateOf.apply(entity);
    if (date == null) {
      throw new CodeGenerationException(
          "Code pattern requires " + description + " but it is not set");
    }
    return date.format(formatter);
  }
}
