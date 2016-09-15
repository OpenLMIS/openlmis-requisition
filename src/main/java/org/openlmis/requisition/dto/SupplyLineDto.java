package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

public class SupplyLineDto {

  @Getter
  @Setter
  private UUID id;

  @Setter
  private UUID supervisoryNode;

  @Getter
  @Setter
  private String description;

  @Setter
  private UUID program;

  @Setter
  private UUID supplyingFacility;

  public SupervisoryNodeDto getSupervisoryNode() {
    //TODO in Sprint 9
    return null;
  }

  public ProgramDto getProgram() {
    //TODO in Sprint 9
    return null;
  }

  public FacilityDto getSupplyingFacility() {
    //TODO in Sprint 9
    return null;
  }
}
