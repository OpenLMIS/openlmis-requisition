package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.Set;
import java.util.UUID;

@Getter
@Setter
public class OrderableProductDto {
  private UUID id;
  private String productCode;
  private String name;
  private long packSize;
  private long packRoundingThreshold;
  private boolean roundToZero;
  private Set<ProgramProductDto> programs;
  private DispensableDto dispensable;

  /**
   * Returns the number of packs to order. For this OrderableProduct given a desired number of
   * dispensing units, will return the number of packs that should be ordered.
   *
   * @param dispensingUnits # of dispensing units we'd like to order for
   * @return the number of packs that should be ordered.
   */
  public long packsToOrder(long dispensingUnits) {
    if (dispensingUnits <= 0 || packSize == 0) {
      return 0;
    } else {
      long packsToOrder = dispensingUnits / packSize;
      long remainderQuantity = dispensingUnits % packSize;

      if (remainderQuantity > 0 && remainderQuantity >= packRoundingThreshold) {
        packsToOrder += 1;
      }

      if (packsToOrder == 0 && !roundToZero) {
        packsToOrder = 1;
      }

      return packsToOrder;
    }
  }
}
