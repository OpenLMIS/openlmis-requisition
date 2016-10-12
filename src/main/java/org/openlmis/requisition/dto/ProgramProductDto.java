package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class ProgramProductDto {
  private UUID id;
  private ProgramDto program;
  private OrderableProductDto product;
  private Integer dosesPerMonth;
  private boolean active;
  private ProductCategoryDto productCategory;
  private String productCategoryDisplayName;
  private int productCategoryDisplayOrder;
  private boolean fullSupply;
  private int displayOrder;
  private int maxMonthsStock;
}
