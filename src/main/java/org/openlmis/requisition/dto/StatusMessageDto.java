package org.openlmis.requisition.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;

import java.time.LocalDateTime;
import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class StatusMessageDto implements StatusMessage.Exporter {
  private UUID id;
  private UUID requisitionId;
  private UUID authorId;
  private RequisitionStatus status;
  private String body;
  private LocalDateTime createdDate;
}
