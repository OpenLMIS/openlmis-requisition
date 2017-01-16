package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.openlmis.requisition.domain.OpenLmisNumberUtils.zeroIfNull;

import org.junit.Test;

import java.math.BigDecimal;

public class OpenLmisNumberUtilsTest {

  @Test
  public void shouldReturnZeroIfValueIsNull() throws Exception {
    assertThat(zeroIfNull((Integer) null), is(equalTo(0)));
    assertThat(zeroIfNull((Long) null), is(equalTo(0L)));
    assertThat(zeroIfNull((BigDecimal) null), is(equalTo(BigDecimal.ZERO)));
  }

  @Test
  public void shouldReturnValueIfItIsNotNull() throws Exception {
    assertThat(zeroIfNull(10), is(equalTo(10)));
    assertThat(zeroIfNull(12345678901L), is(equalTo(12345678901L)));
    assertThat(zeroIfNull(new BigDecimal("7")), is(equalTo(new BigDecimal("7"))));
  }
}
