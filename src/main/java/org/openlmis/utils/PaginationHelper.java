package org.openlmis.utils;

import org.openlmis.requisition.dto.RequisitionDto;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;

@Component
public class PaginationHelper {
  /**
   * Get items from given page of list.
   */
  public List<RequisitionDto> pageCollection(List<RequisitionDto> requisitions,
                                             int pageNumber, int pageSize) {
    int firstPageRecordListIndex = (pageNumber - 1) * pageSize;
    int lastPlusOnePageRecordListIndex = (pageNumber * pageSize);

    if (firstPageRecordListIndex >= requisitions.size()) {
      return Collections.emptyList();
    }
    if (lastPlusOnePageRecordListIndex > requisitions.size()) {
      lastPlusOnePageRecordListIndex = requisitions.size();
    }

    return requisitions.subList(firstPageRecordListIndex, lastPlusOnePageRecordListIndex);
  }
}
