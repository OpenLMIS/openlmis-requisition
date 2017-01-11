package org.openlmis.requisition.domain;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.math.NumberUtils;

import java.math.BigDecimal;

public final class NumberUtil extends NumberUtils {

  private NumberUtil() {
    throw new UnsupportedOperationException();
  }

  static int zeroIfNull(Integer value) {
    return defaultIfNull(value, 0);
  }

  static long zeroIfNull(Long value) {
    return defaultIfNull(value, 0L);
  }

  static BigDecimal zeroIfNull(BigDecimal value) {
    return defaultIfNull(value, BigDecimal.ZERO);
  }

  private static <N extends Number> N defaultIfNull(N number, N defaultNumber) {
    return ObjectUtils.defaultIfNull(number, defaultNumber);
  }

}
