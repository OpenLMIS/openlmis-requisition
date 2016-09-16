package org.openlmis.requisition.repository.custom;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;

import java.time.LocalDateTime;
import java.util.List;

public interface RequisitionRepositoryCustom {

  List<Requisition> searchRequisitions(Facility facility, Program program,
                                       LocalDateTime createdDateFrom,
                                       LocalDateTime createdDateTo,
                                       ProcessingPeriod processingPeriod,
                                       SupervisoryNode supervisoryNode,
                                       RequisitionStatus requisitionStatus);

  List<Requisition> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      String filterValue, String filterBy, String sortBy, Boolean descending,
      Integer pageNumber, Integer pageSize);
}
