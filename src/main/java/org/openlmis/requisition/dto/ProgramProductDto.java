package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Data;

@Data
public class ProgramProductDto {
  private UUID id;
  private ProgramDto program;
  private OrderableProductDto product;
  private Integer dosesPerMonth;
  private boolean active;
  private ProductCategoryDto productCategory;
  private boolean fullSupply;
  private int displayOrder;
  private int maxMonthsStock;
}
