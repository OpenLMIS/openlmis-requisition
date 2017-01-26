package org.openlmis.utils;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;

public class MoneyDeserializerTest {

  private ObjectMapper mapper;
  private MoneyDeserializer moneyDeserializer;

  @Before
  public void setup() {
    mapper = new ObjectMapper();
    moneyDeserializer = new MoneyDeserializer();
    ReflectionTestUtils.setField(moneyDeserializer, "currencyCode", "USD");
  }

  @Test(expected = NumberFormatException.class)
  public void shouldNotDeserializeMoneyWhenValueEmpty() throws IOException {
    String json = String.format("{\"value\":%s}", "\"\"");
    deserializeMoney(json);
  }

  @Test
  public void shouldDeserializeMoney() throws IOException {
    String json = String.format("{\"value\":%s}", "\"11.33\"");
    Money money = deserializeMoney(json);

    assertEquals(new BigDecimal("11.33"), money.getAmount());
    assertEquals(CurrencyUnit.USD, money.getCurrencyUnit());
  }

  private Money deserializeMoney(String json) throws IOException {
    InputStream stream = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
    JsonParser parser = mapper.getFactory().createParser(stream);
    parser.nextValue();
    parser.nextValue();
    DeserializationContext ctxt = mapper.getDeserializationContext();

    return moneyDeserializer.deserialize(parser, ctxt);
  }
}