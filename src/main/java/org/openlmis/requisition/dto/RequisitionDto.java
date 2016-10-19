package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.RequisitionStatus;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RequisitionDto {
  private UUID id;
  private LocalDateTime createdDate;
  private List<RequisitionLineItemDto> requisitionLineItems;
  private List<CommentDto> comments;
  private FacilityDto facility;
  private ProgramDto program;
  private ProcessingPeriodDto processingPeriod;
  private RequisitionStatus status;
  private Boolean emergency;
  private UUID supplyingFacility;
  private UUID supervisoryNode;
}
