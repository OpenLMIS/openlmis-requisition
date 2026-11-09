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

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Renderer of the built in {@code {seq:width}} token. Builds the counter key from the sequence
 * name and the configured scope dimensions, claims the next value and zero pads it.
 *
 * @param <T> type of entity the code is generated for
 */
final class SequenceSegment<T> implements Function<T, String> {

  private static final String KEY_SEPARATOR = "|";
  private static final String DIMENSION_SEPARATOR = ";";
  private static final String VALUE_SEPARATOR = "=";

  private final String sequenceName;
  private final List<String> scope;
  private final Map<String, Function<T, Object>> dimensions;
  private final CodeSequenceRepository sequences;
  private final String format;

  SequenceSegment(String sequenceName, List<String> scope,
      Map<String, Function<T, Object>> dimensions, CodeSequenceRepository sequences,
      String argument) {
    this.sequenceName = sequenceName;
    this.scope = scope;
    this.dimensions = dimensions;
    this.sequences = sequences;
    this.format = parseFormat(argument);
  }

  @Override
  public String apply(T entity) {
    return String.format(format, sequences.next(scopeKey(entity)));
  }

  String scopeKey(T entity) {
    String key = scope.stream()
        .map(name -> name + VALUE_SEPARATOR + dimensionValue(name, entity))
        .collect(Collectors.joining(DIMENSION_SEPARATOR));
    return sequenceName + KEY_SEPARATOR + key;
  }

  private Object dimensionValue(String name, T entity) {
    Object value = dimensions.get(name).apply(entity);
    if (value == null) {
      throw new CodeGenerationException(
          "Sequence scope dimension '" + name + "' has no value for " + sequenceName);
    }
    return value;
  }

  private static String parseFormat(String argument) {
    if (argument == null || argument.isEmpty()) {
      return "%d";
    }
    int width;
    try {
      width = Integer.parseInt(argument);
    } catch (NumberFormatException ex) {
      throw new CodeGenerationException("Sequence width must be a number: " + argument, ex);
    }
    if (width < 1) {
      throw new CodeGenerationException("Sequence width must be positive: " + argument);
    }
    return "%0" + width + "d";
  }
}
