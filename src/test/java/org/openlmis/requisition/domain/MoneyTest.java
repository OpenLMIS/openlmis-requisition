package org.openlmis.requisition.domain;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

import org.junit.Test;

import java.math.BigDecimal;

public class MoneyTest {

  @Test
  public void testEquals() {
    Money one = new Money("3.14");
    Money two = new Money("3.14");

    assertThat(one, is(two));

    two = new Money("3.1");

    assertThat(one, not(two));
  }

  @Test
  public void shouldRoundToTwoDecimals() {
    Money money = new Money("3.14234252663");
    assertThat(money, is(new Money("3.14")));

    money = new Money("3.14934252663");
    assertThat(money, is(new Money("3.15")));
  }

  @Test
  public void shouldMultiply() {
    Money money = new Money("9.29");

    Money result = money.mul(5);

    assertThat(result, is(new Money("46.45")));

    result = money.mul(10L);

    assertThat(result, is(new Money("92.9")));
  }

  @Test
  public void shouldMultiplyByZero() {
    Money money = new Money("12");

    Money result = money.mul(0);

    assertThat(result, is(new Money(BigDecimal.ZERO)));
  }
}
