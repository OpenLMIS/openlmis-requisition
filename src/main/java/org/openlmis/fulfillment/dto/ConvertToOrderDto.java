package org.openlmis.fulfillment.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class ConvertToOrderDto {

  private UUID requisitionId;
  private UUID supplyingDepotId;
}
