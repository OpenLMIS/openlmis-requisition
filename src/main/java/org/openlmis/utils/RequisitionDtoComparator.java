package org.openlmis.utils;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING;

import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.exception.ValidationMessageException;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import java.util.Comparator;

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
        throw new ValidationMessageException(new Message(ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING,
            compareCondition));
      }
    }
  }
}
