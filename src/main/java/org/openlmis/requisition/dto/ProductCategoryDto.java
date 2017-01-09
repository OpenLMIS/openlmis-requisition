package org.openlmis.requisition.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class ProductCategoryDto {
  private UUID id;
  private CodeDto code;
  private OrderedDisplayValueDto orderedDisplayValue;
}
