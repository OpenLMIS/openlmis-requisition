package org.openlmis.requisition.dto;

import java.util.Set;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

public class SupervisoryNodeDto {

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private String code;

  @Getter
  @Setter
  private String name;

  @Getter
  @Setter
  private String description;

  @Setter
  private UUID facility;

  @Setter
  private UUID parentNode;

  @Setter
  private UUID requisitionGroup;

  @Setter
  private UUID childNodes;

  public Set<SupervisoryNodeDto> getChildNodes() {
    //TODO in Sprint 9
    return null;
  }

  public FacilityDto getFacility() {
    //TODO in Sprint 9
    return null;
  }

  public SupervisoryNodeDto getParentNode() {
    //TODO in Sprint 9
    return null;
  }
}