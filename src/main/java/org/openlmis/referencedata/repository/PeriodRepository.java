package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface PeriodRepository extends PagingAndSortingRepository<Period, UUID>{

    Iterable<Period> findByProcessingSchedule(@Param("processingSchedule") Schedule processingScheduleId);
}
