package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Data;

@Data
public class FacilityTypeApprovedProductDto {
  private UUID id;
  private FacilityTypeDto facilityType;
  private ProgramProductDto programProduct;
  private Double maxMonthsOfStock;
  private Double minMonthsOfStock;
  private Double emergencyOrderPoint;
}
