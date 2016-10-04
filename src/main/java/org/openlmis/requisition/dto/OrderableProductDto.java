package org.openlmis.requisition.dto;

import java.util.UUID;

import lombok.Data;

@Data
public class OrderableProductDto {
  private UUID id;
  private String productCode;
  private String name;
}
