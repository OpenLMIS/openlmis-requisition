package org.openlmis.utils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.openlmis.CurrencyConfig;

import java.io.IOException;

/**
 * MoneyDeserializer class represents the deserializer for Joda-Money.
 */
public class MoneyDeserializer extends JsonDeserializer<Money> {

  @Override
  public Money deserialize(JsonParser jsonParser, DeserializationContext ctxt)
      throws IOException {
    return Money.parse(
        CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE).getCode() + " " + jsonParser.getText());
  }
}

