package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RestResource;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface PeriodRepository extends PagingAndSortingRepository<Period, UUID>{

    Iterable<Period> findByProcessingSchedule(@Param("processingSchedule") Schedule processingScheduleId);

    @Override
    @RestResource(exported = false)
    void delete(Period entity);

    @Override
    @RestResource(exported = false)
    void delete(Iterable<? extends Period> entities);

    @Override
    @RestResource(exported = false)
    <S extends Period> S save(S entity);

    @Override
    @RestResource(exported = false)
    <S extends Period> Iterable<S> save(Iterable<S> entities);
}
