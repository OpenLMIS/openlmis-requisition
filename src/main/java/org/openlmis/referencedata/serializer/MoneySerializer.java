package org.openlmis.referencedata.serializer;


import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.openlmis.referencedata.domain.Money;

import java.io.IOException;

/**
 * MoneySerializer class represents the serializer for Money.
 */

public class MoneySerializer extends JsonSerializer<Money> {

  @Override
  public void serialize(Money value, JsonGenerator generator, SerializerProvider provider) throws IOException, JsonProcessingException {
    generator.writeString(value.toString());
  }
}
