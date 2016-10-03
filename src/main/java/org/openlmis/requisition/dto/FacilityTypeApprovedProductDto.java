package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FacilityTypeApprovedProductDto {
  private UUID id;
  private FacilityTypeDto facilityType;
  private ProgramProductDto programProduct;
  private Double maxMonthsOfStock;
  private Double minMonthsOfStock;
  private Double emergencyOrderPoint;
}
