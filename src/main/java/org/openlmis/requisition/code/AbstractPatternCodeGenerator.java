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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates human-readable codes from a configurable pattern such as
 * {@code PREFIX-{facilityCode}-{date:yyyyMM}-{seq:4}}.
 * The pattern is compiled once by {@link #compile()}. Unknown tokens, unknown scope dimensions and
 * invalid token arguments fail the compilation, so a bad configuration stops the application at
 * startup instead of failing on the first generated code.
 *
 * <p>The {@code {seq}} token is built in. It claims the next value of a counter whose key is
 * built from the configured sequence scope, for example
 * {@code <sequenceName>|facility=...;year=2026;month=10}, and zero pads it to the width given as
 * the argument. An empty scope means one counter for the whole sequence name.
 *
 * @param <T> type of entity the code is generated for
 */
public abstract class AbstractPatternCodeGenerator<T> implements CodeGenerator<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(AbstractPatternCodeGenerator.class);

  static final String SEQUENCE_TOKEN = "seq";
  private static final Pattern TOKEN = Pattern.compile("\\{([a-zA-Z][a-zA-Z0-9]*)(?::([^{}]*))?}");

  private final String sequenceName;
  private final String pattern;
  private final List<String> sequenceScope;
  private final CodeSequenceRepository sequences;

  private final Map<String, PatternToken<T>> tokens = new LinkedHashMap<>();
  private final Map<String, Function<T, Object>> dimensions = new LinkedHashMap<>();

  private List<Function<T, String>> segments;

  /**
   * Creates a generator.
   *
   * @param sequenceName stable name that separates counters of this generator from counters of
   *                     other generators in the same schema; changing it restarts numbering
   * @param pattern configured code pattern
   * @param sequenceScope configured names of dimensions the counter is scoped by, in order
   * @param sequences source of sequence values
   */
  protected AbstractPatternCodeGenerator(String sequenceName, String pattern,
      List<String> sequenceScope, CodeSequenceRepository sequences) {
    if (sequenceName == null || sequenceName.trim().isEmpty()) {
      throw new CodeGenerationException("Sequence name must not be blank");
    }
    if (pattern == null || pattern.trim().isEmpty()) {
      throw new CodeGenerationException("Code pattern for " + sequenceName + " must not be blank");
    }
    this.sequenceName = sequenceName;
    this.pattern = pattern;
    this.sequenceScope = sequenceScope == null
        ? Collections.emptyList()
        : Collections.unmodifiableList(new ArrayList<>(sequenceScope));
    this.sequences = sequences;
  }

  /**
   * Splits a comma separated property value into a list of trimmed, non-empty names.
   */
  protected static List<String> splitScope(String value) {
    if (value == null) {
      return Collections.emptyList();
    }
    List<String> result = new ArrayList<>();
    for (String part : value.split(",")) {
      if (!part.trim().isEmpty()) {
        result.add(part.trim());
      }
    }
    return result;
  }

  /**
   * Registers a token the pattern may use.
   */
  protected final void token(String name, PatternToken<T> token) {
    assertNotCompiled();
    if (SEQUENCE_TOKEN.equals(name) || tokens.containsKey(name)) {
      throw new CodeGenerationException("Token '" + name + "' is already registered");
    }
    tokens.put(name, token);
  }

  /**
   * Registers a dimension the sequence may be scoped by. The accessor must return a stable raw
   * value, such as an id or a number, never formatted text.
   */
  protected final void dimension(String name, Function<T, Object> accessor) {
    assertNotCompiled();
    if (dimensions.containsKey(name)) {
      throw new CodeGenerationException("Dimension '" + name + "' is already registered");
    }
    dimensions.put(name, accessor);
  }

  /**
   * Parses the pattern into literal and token segments and validates the configuration. Called
   * by Spring after construction; tests creating the generator manually must call it themselves.
   */
  @PostConstruct
  public final void compile() {
    assertNotCompiled();
    for (String name : sequenceScope) {
      if (!dimensions.containsKey(name)) {
        throw new CodeGenerationException("Unknown sequence scope dimension '" + name + "' for "
            + sequenceName + ". Supported: " + dimensions.keySet());
      }
    }

    List<Function<T, String>> compiled = new ArrayList<>();
    boolean hasSequence = false;
    boolean hasRandom = false;
    Matcher matcher = TOKEN.matcher(pattern);
    int position = 0;
    while (matcher.find()) {
      addLiteral(compiled, pattern.substring(position, matcher.start()));
      String name = matcher.group(1);
      String argument = matcher.group(2);
      if (SEQUENCE_TOKEN.equals(name)) {
        compiled.add(new SequenceSegment<>(sequenceName, sequenceScope, dimensions, sequences,
            argument));
        hasSequence = true;
      } else {
        PatternToken<T> token = tokens.get(name);
        if (token == null) {
          throw new CodeGenerationException("Unknown token '" + name + "' in code pattern '"
              + pattern + "'. Supported: " + tokens.keySet() + " and " + SEQUENCE_TOKEN);
        }
        compiled.add(token.bind(argument));
        hasRandom |= token instanceof RandomToken;
      }
      position = matcher.end();
    }
    addLiteral(compiled, pattern.substring(position));

    if (!hasSequence && !hasRandom) {
      throw new CodeGenerationException("Code pattern '" + pattern
          + "' must contain {seq} or {random} to produce unique codes");
    }
    if (!hasSequence && !sequenceScope.isEmpty()) {
      LOGGER.warn("Sequence scope {} is configured for {} but pattern '{}' has no {seq} token",
          sequenceScope, sequenceName, pattern);
    }
    segments = Collections.unmodifiableList(compiled);
    LOGGER.info("Code pattern for {}: '{}', sequence scope: {}",
        sequenceName, pattern, sequenceScope);
  }

  @Override
  public final String generate(T entity) {
    if (segments == null) {
      throw new IllegalStateException("Code generator for " + sequenceName + " is not compiled");
    }
    StringBuilder code = new StringBuilder();
    for (Function<T, String> segment : segments) {
      code.append(segment.apply(entity));
    }
    return code.toString();
  }

  private void addLiteral(List<Function<T, String>> compiled, String literal) {
    if (literal.isEmpty()) {
      return;
    }
    if (literal.indexOf('{') >= 0 || literal.indexOf('}') >= 0) {
      throw new CodeGenerationException("Malformed token near '" + literal
          + "' in code pattern '" + pattern + "'");
    }
    compiled.add(entity -> literal);
  }

  private void assertNotCompiled() {
    if (segments != null) {
      throw new IllegalStateException("Code generator for " + sequenceName + " is compiled");
    }
  }
}
