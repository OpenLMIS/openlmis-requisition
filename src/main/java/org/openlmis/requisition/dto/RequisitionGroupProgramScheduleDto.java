package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class RequisitionGroupProgramScheduleDto {
  private UUID id;
  private ProgramDto program;
  private ProcessingScheduleDto processingSchedule;
  private Boolean directDelivery;
  private FacilityDto dropOffFacility;
}
