package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ProductCategoryDto {
  private UUID id;
  private CodeDto code;
  private OrderedDisplayValueDto orderedDisplayValue;
}
