package org.openlmis.requisition.dto;


import static java.math.BigDecimal.ROUND_HALF_UP;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Getter;
import lombok.Setter;
import org.openlmis.utils.MoneyDtoDeserializer;
import org.openlmis.utils.MoneyDtoSerializer;

import java.math.BigDecimal;

@Getter
@Setter
@JsonSerialize(using = MoneyDtoSerializer.class)
@JsonDeserialize(using = MoneyDtoDeserializer.class)
public class MoneyDto extends Number {

  private BigDecimal value;

  public MoneyDto(String value) {
    this.value = new BigDecimal(value).setScale(2, ROUND_HALF_UP);
  }

  public MoneyDto(BigDecimal value) {
    this(value.toString());
  }

  @Override
  public String toString() {
    return value.toString();
  }

  @Override
  public int intValue() {
    return value.toBigInteger().intValue();
  }

  @Override
  public long longValue() {
    return value.toBigInteger().longValue();
  }

  @Override
  public float floatValue() {
    return value.floatValue();
  }

  @Override
  public double doubleValue() {
    return value.doubleValue();
  }
}
