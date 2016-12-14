package org.openlmis.utils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.openlmis.requisition.domain.Money;

import java.io.IOException;

/**
 * MoneyDeserializer class represents the deserializer for Money.
 */

public class MoneyDeserializer extends JsonDeserializer<Money> {

  @Override
  public Money deserialize(JsonParser jsonParser, DeserializationContext ctxt)
      throws IOException {
    return new Money(jsonParser.getText());
  }
}

