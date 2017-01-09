package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.Money;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class ProductDto {
  private UUID programId;
  private UUID productId;
  private UUID productCategoryId;
  private String productCategoryDisplayName;
  private Integer productCategoryDisplayOrder;
  private Boolean active;
  private Boolean fullSupply;
  private Integer displayOrder;
  private Integer maxMonthsOfStock;
  private Integer dosesPerMonth;
  private Money pricePerPack;
}
