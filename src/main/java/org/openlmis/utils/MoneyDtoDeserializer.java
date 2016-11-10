package org.openlmis.utils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.openlmis.requisition.dto.MoneyDto;

import java.io.IOException;

/**
 * MoneyDtoDeserializer class represents the deserializer for MoneyDto.
 */

public class MoneyDtoDeserializer extends JsonDeserializer<MoneyDto> {

  @Override
  public MoneyDto deserialize(JsonParser jsonParser, DeserializationContext ctxt)
      throws IOException {
    return new MoneyDto(jsonParser.getText());
  }
}
