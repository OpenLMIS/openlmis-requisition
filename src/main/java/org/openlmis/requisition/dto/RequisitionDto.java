package org.openlmis.requisition.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.domain.RequisitionStatus;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RequisitionDto {
  private UUID id;
  private LocalDateTime createdDate;
  private List<RequisitionLineItemDto> requsitionLineItems;
  private List<Comment> comments;
  private FacilityDto facility;
  private ProgramDto program;
  private ProcessingPeriodDto processingPeriod;
  private RequisitionStatus status;
  private Boolean emergency;
  private UUID supplyingFacility;
  private UUID supervisoryNode;
}
