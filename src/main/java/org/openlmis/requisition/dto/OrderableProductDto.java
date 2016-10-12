package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.UUID;

@Getter
@Setter
public class OrderableProductDto {
  private UUID id;
  private String productCode;
  private String name;
  private List<ProgramProductDto> programs;
}
