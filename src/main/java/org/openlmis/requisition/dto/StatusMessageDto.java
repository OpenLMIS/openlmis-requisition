package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.ZonedDateTime;
import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class StatusMessageDto implements StatusMessage.Exporter {
  private UUID id;
  private UUID requisitionId;
  private UUID authorId;
  private String authorFirstName;
  private String authorLastName;
  private RequisitionStatus status;
  private String body;
  private ZonedDateTime createdDate;
}
