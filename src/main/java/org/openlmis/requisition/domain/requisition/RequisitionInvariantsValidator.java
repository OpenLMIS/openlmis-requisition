/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.domain.requisition;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.BooleanUtils.isNotTrue;
import static org.openlmis.requisition.domain.requisition.Requisition.EMERGENCY_FIELD;
import static org.openlmis.requisition.domain.requisition.Requisition.FACILITY_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.NUMBER_OF_MONTHS_IN_PERIOD;
import static org.openlmis.requisition.domain.requisition.Requisition.PROCESSING_PERIOD_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.PROGRAM_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.domain.requisition.Requisition.SUPERVISORY_NODE_ID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_INVARIANT;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_LINE_ITEM_ADDED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_LINE_ITEM_REMOVED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_BASED_VALUE_MODIFIED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;
import static org.springframework.util.CollectionUtils.isEmpty;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import org.apache.commons.beanutils.PropertyUtils;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.utils.Message;

@AllArgsConstructor
class RequisitionInvariantsValidator
    implements RequisitionUpdateDomainValidator, RequisitionStatusChangeDomainValidator {

  static final String EXTRA_DATA_ORIGINAL_REQUISITION = "extraData.originalRequisition";

  private Requisition requisitionUpdater;
  private Requisition requisitionToUpdate;

  @Override
  public boolean isForRegularOnly() {
    return false;
  }

  @Override
  public boolean isForApprove() {
    return true;
  }

  @Override
  public void validateCanUpdate(Map<String, Message> errors) {
    rejectIfValueChanged(errors, requisitionUpdater.getFacilityId(),
        requisitionToUpdate.getFacilityId(), FACILITY_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getProgramId(),
        requisitionToUpdate.getProgramId(), PROGRAM_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getProcessingPeriodId(),
        requisitionToUpdate.getProcessingPeriodId(), PROCESSING_PERIOD_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getEmergency(),
        requisitionToUpdate.getEmergency(), EMERGENCY_FIELD);
    rejectIfValueChanged(errors, requisitionUpdater.getSupervisoryNodeId(),
        requisitionToUpdate.getSupervisoryNodeId(), SUPERVISORY_NODE_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getNumberOfMonthsInPeriod(),
        requisitionToUpdate.getNumberOfMonthsInPeriod(), NUMBER_OF_MONTHS_IN_PERIOD);

    if (isNotTrue(requisitionToUpdate.getEmergency())) {
      validateRegularLineItemSize(errors);
    }

    if (requisitionToUpdate.getTemplate().isPopulateStockOnHandFromStockCards()) {
      validateRegularLineItemStockFields(errors);
    }

    validateExtraData(errors);
    validateIfOrderableIdChanged(errors);
  }

  @Override
  public void validateCanChangeStatus(Map<String, Message> errors) {
    if (isEmpty(requisitionToUpdate.getNonSkippedRequisitionLineItems())) {
      errors.put(REQUISITION_LINE_ITEMS,
          new Message(ERROR_VALUE_MUST_BE_ENTERED, REQUISITION_LINE_ITEMS));
    }
  }

  private void validateIfOrderableIdChanged(Map<String, Message> errors) {
    Map<UUID, RequisitionLineItem> existingLineItems = requisitionToUpdate
        .getRequisitionLineItems()
        .stream()
        .collect(Collectors.toMap(BaseEntity::getId, Function.identity()));

    Map<UUID, RequisitionLineItem> currentLineItems = requisitionUpdater
        .getRequisitionLineItems()
        .stream()
        // we skip new line items because it's impossible to
        // match them with existing line items.
        .filter(line -> Objects.nonNull(line.getId()))
        .collect(Collectors.toMap(BaseEntity::getId, Function.identity()));

    for (Map.Entry<UUID, RequisitionLineItem> entry : existingLineItems.entrySet()) {
      RequisitionLineItem existing = entry.getValue();
      RequisitionLineItem current = currentLineItems.get(entry.getKey());

      if (null != current) {
        rejectIfValueChanged(errors, current.getOrderableId(),
            existing.getOrderableId(), REQUISITION_LINE_ITEMS);
      }
    }
  }

  private void validateRegularLineItemSize(Map<String, Message> errors) {
    List<UUID> currentIds = requisitionUpdater
        .getRequisitionLineItems()
        .stream()
        .filter(line -> !line.isNonFullSupply())
        .map(BaseEntity::getId)
        .collect(toList());

    List<UUID> existingIds = requisitionToUpdate
        .getRequisitionLineItems()
        .stream()
        .filter(line -> !line.isNonFullSupply())
        .map(BaseEntity::getId)
        .collect(toList());

    if (currentIds.stream().anyMatch(id -> !existingIds.contains(id))) {
      errors.put(REQUISITION_LINE_ITEMS, new Message(ERROR_LINE_ITEM_ADDED));
    } else if (existingIds.stream().anyMatch(id -> !currentIds.contains(id))) {
      errors.put(REQUISITION_LINE_ITEMS, new Message(ERROR_LINE_ITEM_REMOVED));
    }
  }

  private void validateRegularLineItemStockFields(Map<String, Message> errors) {
    RequisitionTemplate template = requisitionToUpdate.getTemplate();
    Map<String, RequisitionTemplateColumn> columns = template.viewColumns();

    for (Entry<String, RequisitionTemplateColumn> column : columns.entrySet()) {
      if (column.getValue().getSource().isStockSource()) {
        validateRegularLineItemStockField(errors, column.getKey());
      }
    }
  }

  private void validateRegularLineItemStockField(Map<String, Message> errors, String columnName) {
    Map<UUID, Object> columnValues = requisitionToUpdate
        .getFullSupplyRequisitionLineItems()
        .stream()
        .collect(
            HashMap::new,
            (map, line) -> map.put(line.getOrderableId(), getColumnValue(line, columnName)),
            HashMap::putAll
        );

    requisitionUpdater
        .getFullSupplyRequisitionLineItems()
        .forEach(line -> {

          Object currentValue = columnValues.get(line.getOrderableId());
          Object newValue = getColumnValue(line, columnName);

          if (!requisitionToUpdate.getTemplate().isColumnDisplayed(columnName)
              && newValue == null) {
            setColumnValue(line, columnName, currentValue);
            return;
          }

          if (!Objects.equals(currentValue, newValue)) {
            errors.put(
                REQUISITION_LINE_ITEMS,
                new Message(
                    ERROR_STOCK_BASED_VALUE_MODIFIED, columnName,
                    String.valueOf(currentValue), String.valueOf(newValue)
                )
            );
          }
        });
  }

  private void validateExtraData(Map<String, Message> errors) {
    if (requisitionUpdater.hasOriginalRequisitionId()) {
      rejectIfValueChanged(errors, requisitionUpdater.getOriginalRequisitionId(),
          requisitionToUpdate.getOriginalRequisitionId(), EXTRA_DATA_ORIGINAL_REQUISITION);
    }
  }

  private Object getColumnValue(RequisitionLineItem lineItem, String columnName) {
    try {
      return PropertyUtils.getProperty(lineItem, columnName);
    } catch (IllegalAccessException | NoSuchMethodException | InvocationTargetException exp) {
      throw new IllegalStateException(exp);
    }
  }

  private void rejectIfValueChanged(Map<String, Message> errors, Object value,
      Object savedValue, String field) {
    if (value != null && savedValue != null && !savedValue.equals(value)) {
      errors.put(field, new Message(ERROR_IS_INVARIANT, field));
    }
  }

  private void setColumnValue(RequisitionLineItem lineItem, String columnName, Object value) {
    try {
      PropertyUtils.setProperty(lineItem, columnName, value);
    } catch (IllegalAccessException | NoSuchMethodException | InvocationTargetException exp) {
      throw new IllegalStateException(exp);
    }
  }

}
