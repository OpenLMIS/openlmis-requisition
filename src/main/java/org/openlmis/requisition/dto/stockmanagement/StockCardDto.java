package org.openlmis.requisition.dto.stockmanagement;

import org.openlmis.requisition.dto.BaseDto;
import org.openlmis.requisition.dto.OrderableDto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Builder
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class StockCardDto {
  private BaseDto lot;
  private OrderableDto orderable;
  private Integer stockOnHand;
}
