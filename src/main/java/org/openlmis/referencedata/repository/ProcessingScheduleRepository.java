package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface ProcessingScheduleRepository
      extends PagingAndSortingRepository<ProcessingSchedule, UUID> {

}
