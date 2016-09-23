package org.openlmis.requisition.dto;

import java.util.Set;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SupervisoryNodeDto {
  private UUID id;
  private String code;
  private String name;
  private String description;
  private FacilityDto facility;
  private SupervisoryNodeDto parentNode;
  private UUID requisitionGroup; //TODO
  private Set<SupervisoryNodeDto> childNodes;
}