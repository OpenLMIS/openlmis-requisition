package org.openlmis.utils;

import org.openlmis.requisition.dto.RequisitionDto;

import java.util.Comparator;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class RequisitionDtoComparator implements Comparator<RequisitionDto> {

  private String compareCondition;

  @Override
  public int compare(RequisitionDto o1, RequisitionDto o2) {
    switch (compareCondition) {
      case "programName": {
        return o1.getProgram().getName().compareTo(o2.getProgram().getName());
      }
      case "facilityCode": {
        return o1.getFacility().getCode().compareTo(o2.getFacility().getCode());
      }
      case "facilityName": {
        return o1.getFacility().getName().compareTo(o2.getFacility().getName());
      }
      default: {
        throw new IllegalArgumentException(
            compareCondition + " is not a valid column for sorting");
      }
    }
  }
}
