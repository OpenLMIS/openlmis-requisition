package org.openlmis.requisition.repository;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface RequisitionRepository
    extends PagingAndSortingRepository<Requisition, UUID> {

  Requisition findByProcessingPeriodAndFacilityAndProgram(
      @Param("processingPeriod") Period processingPeriod,
      @Param("facility")Facility facility,
      @Param("program")Program program);

  Iterable<Requisition> findBySupervisoryNode(@Param("supervisoryNodeId")
                                                  SupervisoryNode supervisoryNode);
  Iterable<Requisition> findByStatus(@Param("status") RequisitionStatus status);
}
