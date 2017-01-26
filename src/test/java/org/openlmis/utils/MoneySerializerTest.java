package org.openlmis.utils;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Test;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

public class MoneySerializerTest {

  @Test
  public void shouldSerializeMoney() throws IOException {
    Money money = Money.of(CurrencyUnit.USD, 11.23);

    Writer jsonWriter = new StringWriter();
    JsonGenerator jsonGenerator = new JsonFactory().createGenerator(jsonWriter);
    SerializerProvider serializerProvider = new ObjectMapper().getSerializerProvider();

    MoneySerializer moneySerializer = new MoneySerializer();
    moneySerializer.serialize(money, jsonGenerator, serializerProvider);
    jsonGenerator.flush();

    assertEquals("11.23", jsonWriter.toString());
  }
}