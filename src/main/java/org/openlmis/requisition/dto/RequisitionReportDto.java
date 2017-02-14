package org.openlmis.requisition.dto;

import java.time.ZonedDateTime;
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
  private UserDto initiatedBy;
  private ZonedDateTime initiatedDate;
  private UserDto submittedBy;
  private ZonedDateTime submittedDate;
  private UserDto authorizedBy;
  private ZonedDateTime authorizedDate;
}
