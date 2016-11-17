package org.openlmis.utils;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.openlmis.requisition.dto.MoneyDto;

import java.io.IOException;

/**
 * MoneyDtoSerializer class represents the serializer for MoneyDto.
 */

public class MoneyDtoSerializer extends JsonSerializer<MoneyDto> {

  @Override
  public void serialize(MoneyDto value, JsonGenerator generator, SerializerProvider provider)
      throws IOException, JsonProcessingException {
    generator.writeString(value.toString());
  }
}

