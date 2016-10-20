package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;

import java.util.HashMap;
import java.util.Map;

public class RequisitionValidationTestUtils {

  static Map<String, RequisitionTemplateColumn> initiateColumns() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    columns.put(RequisitionLineItem.REQUESTED_QUANTITY,
        generateTemplateColumn(RequisitionLineItem.REQUESTED_QUANTITY,
            SourceType.USER_INPUT, "J"));
    columns.put(RequisitionLineItem.TOTAL_RECEIVED_QUANTITY,
        generateTemplateColumn(RequisitionLineItem.TOTAL_RECEIVED_QUANTITY,
            SourceType.USER_INPUT, "B"));
    columns.put(RequisitionLineItem.STOCK_ON_HAND,
        generateTemplateColumn(RequisitionLineItem.STOCK_ON_HAND, SourceType.USER_INPUT, "E"));
    columns.put(RequisitionLineItem.BEGINNING_BALANCE,
        generateTemplateColumn(RequisitionLineItem.BEGINNING_BALANCE, SourceType.USER_INPUT, "A"));
    columns.put(RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION,
        generateTemplateColumn(RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION,
            SourceType.USER_INPUT, "W"));
    columns.put(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY,
        generateTemplateColumn(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY,
            SourceType.USER_INPUT, "C"));
    columns.put(RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS,
        generateTemplateColumn(RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS,
            SourceType.USER_INPUT, "D"));
    return columns;
  }

  private static RequisitionTemplateColumn generateTemplateColumn(
      String name, SourceType sourceType, String indicator) {
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setName(name);
    columnDefinition.setIndicator(indicator);

    RequisitionTemplateColumn requisitionTemplateColumn = new RequisitionTemplateColumn();
    requisitionTemplateColumn.setColumnDefinition(columnDefinition);
    requisitionTemplateColumn.setSource(sourceType);
    requisitionTemplateColumn.setName(name);
    requisitionTemplateColumn.setIsDisplayed(true);
    return requisitionTemplateColumn;
  }
}
