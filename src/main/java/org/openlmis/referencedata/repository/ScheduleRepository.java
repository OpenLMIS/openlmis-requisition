package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Schedule;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface ScheduleRepository extends PagingAndSortingRepository<Schedule, UUID>{

}
