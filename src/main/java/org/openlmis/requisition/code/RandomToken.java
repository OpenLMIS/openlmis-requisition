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

import java.math.BigInteger;
import java.util.UUID;
import java.util.function.Function;

/**
 * Renders random upper case base 36 characters, for example {@code {random:8}}. The default
 * length is {@value #DEFAULT_LENGTH}. Uniqueness is probabilistic, so prefer {@code {seq}} when
 * codes must never collide.
 *
 * @param <T> type of entity the code is generated for
 */
public final class RandomToken<T> implements PatternToken<T> {

  static final int DEFAULT_LENGTH = 8;
  static final int MAX_LENGTH = 24;
  private static final int FULL_LENGTH = 25;

  @Override
  public Function<T, String> bind(String argument) {
    int length = parseLength(argument);
    return entity -> render(length);
  }

  @Override
  public String apply(T entity, String argument) {
    return render(parseLength(argument));
  }

  private static int parseLength(String argument) {
    if (argument == null || argument.isEmpty()) {
      return DEFAULT_LENGTH;
    }
    int length;
    try {
      length = Integer.parseInt(argument);
    } catch (NumberFormatException ex) {
      throw new CodeGenerationException("Random token length must be a number: " + argument, ex);
    }
    if (length < 1 || length > MAX_LENGTH) {
      throw new CodeGenerationException(
          "Random token length must be between 1 and " + MAX_LENGTH + ": " + argument);
    }
    return length;
  }

  private static String render(int length) {
    String hex = UUID.randomUUID().toString().replace("-", "");
    String base36 = new BigInteger(hex, 16).toString(36).toUpperCase();
    StringBuilder padded = new StringBuilder(base36);
    while (padded.length() < FULL_LENGTH) {
      padded.insert(0, '0');
    }
    // the leading character of a 128-bit number is not uniformly distributed, take the tail
    return padded.substring(FULL_LENGTH - length);
  }
}
