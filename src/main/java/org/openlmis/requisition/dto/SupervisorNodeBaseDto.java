package org.openlmis.requisition.dto;


import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SupervisorNodeBaseDto {

  private String code;
  private FacilityDto facility;
  private String name;
  private String description;
}
