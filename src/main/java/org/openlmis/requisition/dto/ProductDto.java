package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ProductDto {
  private UUID id;
  private String code;
  private String primaryName;
  private String dispensingUnit;
  private Integer dosesPerDispesingUnit;
  private Integer packSize;
  private Integer packRoundingThreshold;
  private Boolean roundTozero;
  private Boolean active;
  private Boolean fullSupply;
  private Boolean tracer;
}
