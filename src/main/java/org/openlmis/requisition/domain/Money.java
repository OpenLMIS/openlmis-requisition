package org.openlmis.requisition.domain;


import static java.math.BigDecimal.ROUND_HALF_UP;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Getter;
import lombok.Setter;
import org.openlmis.utils.MoneyDeserializer;
import org.openlmis.utils.MoneySerializer;

import java.math.BigDecimal;
import java.util.Objects;

@Getter
@Setter
@JsonSerialize(using = MoneySerializer.class)
@JsonDeserialize(using = MoneyDeserializer.class)
public class Money extends Number {

  public static final Money ZERO = new Money(BigDecimal.ZERO);
  public static final String VALUE_FIELD = "value";

  private BigDecimal value;

  public Money() {
    this("0");
  }

  public Money(String value) {
    this.value = new BigDecimal(value).setScale(2, ROUND_HALF_UP);
  }

  public Money(BigDecimal value) {
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

  /**
   * Multiplication, multiplies the value represented by this money object
   * by the provided value.
   * @param multiplyBy the value to multiply by
   * @return a new Money object representing the result of the multiplication
   */
  public Money mul(int multiplyBy) {
    return new Money(value.multiply(new BigDecimal(multiplyBy)));
  }

  /**
   * Multiplication, multiplies the value represented by this money object
   * by the provided value.
   * @param multiplyBy the value to multiply by
   * @return a new Money object representing the result of the multiplication
   */
  public Money mul(long multiplyBy) {
    return new Money(value.multiply(new BigDecimal(multiplyBy)));
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Money)) {
      return false;
    }

    Money otherMoney = (Money) other;

    return Objects.equals(value, otherMoney.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(value);
  }
}
