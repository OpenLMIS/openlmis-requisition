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

import java.util.UUID;
import java.util.function.Function;

/**
 * Renders a value of an entity referenced by id, for example the code of the facility or program.
 * The same class serves every reference: only the id accessor and the value resolver differ.
 *
 * @param <T> type of entity the code is generated for
 */
public final class LookupToken<T> implements PatternToken<T> {

  private final String description;
  private final Function<T, UUID> idOf;
  private final Function<UUID, String> valueOf;

  /**
   * Creates a token.
   *
   * @param description human-readable name of the referenced value, used in error messages
   * @param idOf returns the id of the referenced entity
   * @param valueOf resolves the rendered value for an id, may return {@code null} when not found
   */
  public LookupToken(String description, Function<T, UUID> idOf,
      Function<UUID, String> valueOf) {
    this.description = description;
    this.idOf = idOf;
    this.valueOf = valueOf;
  }

  @Override
  public Function<T, String> bind(String argument) {
    if (argument != null) {
      throw new CodeGenerationException(
          "Token for " + description + " does not accept an argument");
    }
    return this::render;
  }

  @Override
  public String apply(T entity, String argument) {
    return render(entity);
  }

  private String render(T entity) {
    UUID id = idOf.apply(entity);
    if (id == null) {
      throw new CodeGenerationException(
          "Code pattern requires " + description + " but it is not set");
    }
    String value = valueOf.apply(id);
    if (value == null) {
      throw new CodeGenerationException(
          "Code pattern requires " + description + " but none was found for id " + id);
    }
    return value;
  }
}
