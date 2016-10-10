package org.openlmis.requisition.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class RequisitionWithSupplyingDepotsDto {
  private RequisitionDto requisition;
  private List<FacilityDto> supplyingDepots;
}
