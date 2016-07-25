package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
import java.util.UUID;

public interface PeriodRepository extends ReferenceDataRepository<Period, UUID> {

  Iterable<Period> findByProcessingSchedule(@Param("processingSchedule")
                                                    Schedule processingScheduleId);

  Period findFirst1ByOrderByEndDateDesc();

  @Query("FROM Period p WHERE p.processingSchedule = :processingSchedule "
      + "AND startDate < :startDate ORDER BY startDate DESC")
  Iterable<Period> findPreviousPeriods(@Param("processingSchedule") Schedule processingSchedule,
                                      @Param("startDate")LocalDate startDate);

}
