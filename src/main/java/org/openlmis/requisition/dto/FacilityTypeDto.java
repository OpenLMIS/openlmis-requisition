package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FacilityTypeDto {
  private UUID id;
  private String code;
  private String name;
  private String description;
  private Integer displayOrder;
  private Boolean active;
}
