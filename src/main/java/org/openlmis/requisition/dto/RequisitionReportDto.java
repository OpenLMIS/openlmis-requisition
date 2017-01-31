package org.openlmis.requisition.dto;

import org.joda.money.Money;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class RequisitionReportDto {
  private RequisitionDto requisition;
  private List<RequisitionLineItemDto> fullSupply;
  private List<RequisitionLineItemDto> nonFullSupply;
  private Money fullSupplyTotalCost;
  private Money nonFullSupplyTotalCost;
  private Money totalCost;
  private UserDto createdBy;
  private UserDto submittedBy;
  private UserDto authorizedBy;
}
