package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.RequisitionGroupProgramSchedule;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionGroupProgramScheduleRepository
    extends PagingAndSortingRepository<RequisitionGroupProgramSchedule, UUID> {
}
