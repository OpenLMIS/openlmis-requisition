package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Data;

@Data
public class ProductCategoryDto {
  private UUID id;
  private CodeDto code;
  private OrderedDisplayValueDto orderedDisplayValue;
}
