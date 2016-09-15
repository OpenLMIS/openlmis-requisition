package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;

import java.time.LocalDateTime;
import java.util.List;

public interface RequisitionRepositoryCustom {

  List<Requisition> searchRequisitions(FacilityDto facility, ProgramDto program,
                                       LocalDateTime createdDateFrom,
                                       LocalDateTime createdDateTo,
                                       ProcessingPeriodDto processingPeriod,
                                       SupervisoryNodeDto supervisoryNode,
                                       RequisitionStatus requisitionStatus);
}
