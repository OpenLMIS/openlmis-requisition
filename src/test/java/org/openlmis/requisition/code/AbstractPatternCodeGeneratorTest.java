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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.time.LocalDate;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("PMD.TooManyMethods")
public class AbstractPatternCodeGeneratorTest {

  private static final String SEQ = "{seq}";
  private static final String FACILITY = "facility";

  private static final UUID FACILITY_ID = UUID.randomUUID();
  private static final LocalDate DATE = LocalDate.of(2026, 3, 7);

  private final FakeSequences sequences = new FakeSequences();
  private Item item;

  @Before
  public void setUp() {
    item = new Item(FACILITY_ID, DATE, true);
  }

  @Test
  public void shouldRenderLiteralsAndTokensInOrder() {
    String code = generator("REQ-{facilityCode}-{date:ddMMyyyy}-{seq:4}",
        "facility,year").generate(item);

    assertThat(code).isEqualTo("REQ-HC01-07032026-0001");
  }

  @Test
  public void shouldSupportDifferentDateFormats() {
    assertThat(generator("{date:yyyy-MM-dd}/{seq}", "").generate(item))
        .startsWith("2026-03-07/");
    assertThat(generator("{date}/{seq}", "").generate(item))
        .startsWith("20260307/");
    assertThat(generator("{date:yyMM}'{seq}", "").generate(item))
        .startsWith("2603'");
    assertThat(generator("{date:dd.MM.yy}-{seq}", "").generate(item))
        .startsWith("07.03.26-");
  }

  @Test
  public void shouldIncrementSequencePerScopeKey() {
    AbstractPatternCodeGenerator<Item> generator = generator("{seq:3}", FACILITY);
    Item other = new Item(UUID.randomUUID(), DATE, false);

    assertThat(generator.generate(item)).isEqualTo("001");
    assertThat(generator.generate(item)).isEqualTo("002");
    assertThat(generator.generate(other)).isEqualTo("001");
  }

  @Test
  public void shouldBuildScopeKeyFromSequenceNameAndDimensionsInConfiguredOrder() {
    generator(SEQ, "year,facility").generate(item);

    assertThat(sequences.lastKey).isEqualTo("test|year=2026;facility=" + FACILITY_ID);
  }

  @Test
  public void shouldUseSingleCounterWhenScopeIsEmpty() {
    generator(SEQ, "").generate(item);

    assertThat(sequences.lastKey).isEqualTo("test|");
  }

  @Test
  public void shouldRenderConditionalToken() {
    AbstractPatternCodeGenerator<Item> generator = generator("{seq}{emergency:E|R}", "");

    assertThat(generator.generate(item)).isEqualTo("1E");
    assertThat(generator.generate(new Item(FACILITY_ID, DATE, null))).isEqualTo("2R");
  }

  @Test
  public void shouldRenderRandomTokenOfGivenLength() {
    String code = generator("X{random:12}", "").generate(item);

    assertThat(code).matches("X[0-9A-Z]{12}");
  }

  @Test
  public void shouldFailOnUnknownToken() {
    assertThatThrownBy(() -> generator("{programCode}-{seq}", ""))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("Unknown token 'programCode'");
  }

  @Test
  public void shouldFailOnUnknownScopeDimension() {
    assertThatThrownBy(() -> generator(SEQ, "facility,program"))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("Unknown sequence scope dimension 'program'");
  }

  @Test
  public void shouldFailOnInvalidDateFormat() {
    assertThatThrownBy(() -> generator("{date:ddMMyyyyb}-{seq}", ""))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("Invalid date format");
  }

  @Test
  public void shouldFailOnTimeFieldsInDateFormat() {
    assertThatThrownBy(() -> generator("{date:yyyyMMddHHmm}-{seq}", ""))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("Invalid date format");
  }

  @Test
  public void shouldFailOnMalformedToken() {
    assertThatThrownBy(() -> generator("{date:yyyy-{seq}", ""))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("Malformed token");
  }

  @Test
  public void shouldFailWhenPatternCannotProduceUniqueCodes() {
    assertThatThrownBy(() -> generator("{facilityCode}", ""))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("must contain {seq} or {random}");
  }

  @Test
  public void shouldFailOnInvalidTokenArguments() {
    assertThatThrownBy(() -> generator("{seq:x}", ""))
        .isInstanceOf(CodeGenerationException.class);
    assertThatThrownBy(() -> generator("{seq}{emergency:E}", ""))
        .isInstanceOf(CodeGenerationException.class);
    assertThatThrownBy(() -> generator("{random:99}", ""))
        .isInstanceOf(CodeGenerationException.class);
    assertThatThrownBy(() -> generator("{seq}{facilityCode:x}", ""))
        .isInstanceOf(CodeGenerationException.class);
  }

  @Test
  public void shouldFailWhenReferencedValueIsMissing() {
    AbstractPatternCodeGenerator<Item> generator = generator("{facilityCode}{seq}", "");
    Item withoutFacility = new Item(null, DATE, false);

    assertThatThrownBy(() -> generator.generate(withoutFacility))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("facility code");
  }

  @Test
  public void shouldFailWhenScopeDimensionHasNoValue() {
    AbstractPatternCodeGenerator<Item> generator = generator(SEQ, FACILITY);
    Item withoutFacility = new Item(null, DATE, false);

    assertThatThrownBy(() -> generator.generate(withoutFacility))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("'facility'");
  }

  @Test
  public void shouldSplitScopeProperty() {
    assertThat(AbstractPatternCodeGenerator.splitScope(" facility , ,year "))
        .containsExactly("facility", "year");
    assertThat(AbstractPatternCodeGenerator.splitScope("")).isEmpty();
    assertThat(AbstractPatternCodeGenerator.splitScope(null)).isEmpty();
  }

  @Test
  public void shouldNotAllowRegisteringTokensAfterCompilation() {
    TestGenerator generator = new TestGenerator(SEQ, Collections.emptyList(), sequences);
    generator.compile();

    assertThatThrownBy(() -> generator.lateToken())
        .isInstanceOf(IllegalStateException.class);
  }

  private AbstractPatternCodeGenerator<Item> generator(String pattern, String scope) {
    TestGenerator generator = new TestGenerator(pattern,
        AbstractPatternCodeGenerator.splitScope(scope), sequences);
    generator.compile();
    return generator;
  }

  private static final class TestGenerator extends AbstractPatternCodeGenerator<Item> {

    TestGenerator(String pattern, List<String> scope, CodeSequenceRepository sequences) {
      super("test", pattern, scope, sequences);
      token("facilityCode", new LookupToken<>("facility code", i -> i.facilityId,
          id -> codes().get(id)));
      token("date", new DateToken<>("date", i -> i.date));
      token("emergency", new ConditionalToken<>("emergency flag", i -> i.emergency));
      token("random", new RandomToken<>());
      dimension(FACILITY, i -> i.facilityId);
      dimension("year", i -> i.date.getYear());
    }

    void lateToken() {
      token("late", (entity, argument) -> "");
    }

    private static Map<UUID, String> codes() {
      Map<UUID, String> codes = new HashMap<>();
      codes.put(FACILITY_ID, "HC01");
      return codes;
    }
  }

  private static final class Item {
    private final UUID facilityId;
    private final LocalDate date;
    private final Boolean emergency;

    Item(UUID facilityId, LocalDate date, Boolean emergency) {
      this.facilityId = facilityId;
      this.date = date;
      this.emergency = emergency;
    }
  }

  private static final class FakeSequences implements CodeSequenceRepository {
    private final Map<String, Integer> values = new HashMap<>();
    private String lastKey;

    @Override
    public int next(String scopeKey) {
      lastKey = scopeKey;
      return values.merge(scopeKey, 1, Integer::sum);
    }
  }
}
