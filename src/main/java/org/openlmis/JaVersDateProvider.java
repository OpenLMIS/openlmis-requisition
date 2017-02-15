package org.openlmis;

import org.javers.common.date.DateProvider;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalDateTime;

import java.time.Instant;
import java.time.Duration;
import java.time.ZoneId;
import java.time.ZonedDateTime;

/**
 * This class may be used by JaVers to retrieve the LocalDateTime that it associates with commits.
 * It is intended to be used, rather than JaVers' default DateProvider, so as to be explicit and
 * consistent with the use of UTC within JaVers' domain. (Otherwise, JaVers uses the default
 * system timezone, which may change, when constructing a LocalDateTime.)
 */
public class JaVersDateProvider implements DateProvider {
  public static final DateTimeZone DATE_TIME_ZONE = DateTimeZone.UTC;

  public LocalDateTime now() {
    return LocalDateTime.now(DATE_TIME_ZONE);
  }

  /**
   * Converts the specified LocalDateTime to a ZonedDateTime, given the specified zoneId.
   */
  public static ZonedDateTime getZonedDateTime(LocalDateTime localDateTime, ZoneId zoneId) {

    /* Get an instant representing localDateTime with the understanding that it was stored
       using JaVersDateProvider.DATE_TIME_ZONE */
    long epoch = localDateTime.toDateTime(DATE_TIME_ZONE).getMillis();
    Instant instant = Instant.ofEpochMilli(epoch);

    //Convert the instant to a ZonedDateTime using the specified zoneId.
    return ZonedDateTime.ofInstant( instant, zoneId );
  }

  /**
   * Returns a ZonedDateTime (in UTC!) nearly a billion years prior to the unixEpoch.
   */
  public static ZonedDateTime getMinDateTime() {
    /* The earliest ZonedDateTime value is -999999999 years prior to unixEpoch.
       Instant.MIN, however, falls in the year -1000000000. Therefore, add just
       over a year to it. */
    Instant minInstant = Instant.MIN.plus(Duration.ofDays(366));

    //Because we're returning such an extreme value, its timezone is irrelevant.
    return ZonedDateTime.ofInstant(minInstant, ZoneId.of("UTC"));
  }

  /**
   * Returns a ZonedDateTime (in UTC!) nearly a billion years past the unixEpoch.
   */
  public static ZonedDateTime getMaxDateTime() {
    Instant maxInstant = Instant.MAX.minus(Duration.ofDays(366));
    return ZonedDateTime.ofInstant(maxInstant, ZoneId.of("UTC"));
  }
}
