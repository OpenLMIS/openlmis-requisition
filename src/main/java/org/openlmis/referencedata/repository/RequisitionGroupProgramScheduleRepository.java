package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.RequisitionGroupProgramSchedule;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionGroupProgramScheduleRepository
    extends PagingAndSortingRepository<RequisitionGroupProgramSchedule, UUID> {
}
