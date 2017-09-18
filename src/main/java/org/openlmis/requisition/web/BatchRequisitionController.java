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

package org.openlmis.requisition.web;

import static org.apache.commons.lang3.BooleanUtils.isFalse;

import com.google.common.collect.Lists;

import org.apache.commons.beanutils.PropertyUtils;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.dto.ApproveRequisitionDto;
import org.openlmis.requisition.dto.ApproveRequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionErrorMessage;
import org.openlmis.requisition.dto.RequisitionsProcessingStatusDto;
import org.openlmis.requisition.errorhandling.ValidationFailure;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

@Controller
public class BatchRequisitionController extends BaseRequisitionController {

  @Autowired
  private MessageService messageService;

  /**
   * Attempts to retrieve requisitions with the provided UUIDs.
   */
  @RequestMapping(value = "/requisitions", params = "retrieveAll", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<RequisitionsProcessingStatusDto> retrieveAll(
      @RequestParam(value = "id") List<UUID> uuids) {
    List<Requisition> requisitions = Lists.newArrayList(requisitionRepository.findAll(uuids));

    RequisitionsProcessingStatusDto processingStatus = new RequisitionsProcessingStatusDto();

    for (Requisition requisition : requisitions) {
      ValidationResult accessCheck = permissionService.canViewRequisition(requisition);
      if (accessCheck.hasErrors()) {
        processingStatus.addProcessingError(new RequisitionErrorMessage(requisition.getId(),
                localizeMessage(accessCheck.getError().getMessage())));
      } else {
        processingStatus.addProcessedRequisition(
            new ApproveRequisitionDto(requisitionDtoBuilder.build(requisition)));
      }
    }

    processingStatus.removeSkippedProducts();
    return buildResponse(processingStatus);
  }

  /**
   * Attempts to approve requisitions with the provided UUIDs.
   */
  @RequestMapping(value = "/requisitions", params = "approveAll", method = RequestMethod.POST)
  @ResponseBody
  public ResponseEntity<RequisitionsProcessingStatusDto> approve(
      @RequestParam(value = "id") List<UUID> uuids) {

    RequisitionsProcessingStatusDto processingStatus = new RequisitionsProcessingStatusDto();
    UUID userId = authenticationHelper.getCurrentUser().getId();

    for (UUID requisitionId : uuids) {
      Requisition requisition = requisitionRepository.findOne(requisitionId);

      ValidationResult validationResult =
          requisitionService.validateCanApproveRequisition(requisition, requisitionId, userId);
      if (addValidationErrors(processingStatus, validationResult, requisitionId)) {
        continue;
      }

      validationResult = validateFields(validator, requisition);
      if (addValidationErrors(processingStatus, validationResult, requisitionId)) {
        continue;
      }

      doApprove(requisition, userId);
      processingStatus.addProcessedRequisition(
          new ApproveRequisitionDto(requisitionDtoBuilder.build(requisition)));
    }

    return buildResponse(processingStatus);
  }

  /**
   * Attempts to approve requisitions with the provided UUIDs.
   */
  @RequestMapping(value = "/requisitions", params = "saveAll", method = RequestMethod.PUT)
  @ResponseBody
  public ResponseEntity<RequisitionsProcessingStatusDto> update(
      @RequestBody List<ApproveRequisitionDto> dtos) {

    RequisitionsProcessingStatusDto processingStatus = new RequisitionsProcessingStatusDto();

    for (ApproveRequisitionDto dto : dtos) {
      ValidationResult result = requisitionService.validateCanSaveRequisition(dto.getId());
      if (addValidationErrors(processingStatus, result, dto.getId())) {
        continue;
      }

      Requisition requisitionToUpdate = requisitionRepository.findOne(dto.getId());
      Requisition requisition = buildRequisition(dto, requisitionToUpdate);

      result = requisitionVersionValidator.validateRequisitionTimestamps(
          requisition, requisitionToUpdate);
      result.addValidationResult(validateFields(draftValidator, requisition));

      if (addValidationErrors(processingStatus, result, dto.getId())) {
        continue;
      }

      processingStatus.addProcessedRequisition(
            new ApproveRequisitionDto(doUpdate(requisitionToUpdate, requisition)));
    }

    processingStatus.removeSkippedProducts();
    return buildResponse(processingStatus);
  }

  private Requisition buildRequisition(ApproveRequisitionDto dto, Requisition requisitionToUpdate) {
    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDtoBuilder.build(requisitionToUpdate),
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        requisitionToUpdate.getStatus());
    requisition.setTemplate(requisitionToUpdate.getTemplate());
    requisition.setId(requisitionToUpdate.getId());
    return updateOne(dto, requisition);
  }

  private Requisition updateOne(ApproveRequisitionDto dto, Requisition requisition) {
    for (ApproveRequisitionLineItemDto line : dto.getRequisitionLineItems()) {
      requisition
          .getRequisitionLineItems()
          .stream()
          .filter(original -> Objects.equals(original.getId(), line.getId()))
          .findFirst()
          .ifPresent(original -> {
            original.setApprovedQuantity(line.getApprovedQuantity());
          });
    }
    requisition.setModifiedDate(dto.getModifiedDate());
    nullCalculatedFields(requisition);
    return requisition;
  }

  private boolean addValidationErrors(RequisitionsProcessingStatusDto processingStatus,
                                      ValidationResult validationResult, UUID id) {
    if (validationResult.hasErrors()) {
      for (ValidationFailure failure : validationResult.gerErrors()) {
        processingStatus.addProcessingError(
            new RequisitionErrorMessage(id, localizeMessage(failure.getMessage()),
                failure.getFieldErrors()));
      }

      return true;
    }

    return false;
  }

  private void nullCalculatedFields(Requisition requisition) {
    for (RequisitionLineItem lineItem : requisition.getRequisitionLineItems()) {
      for (RequisitionTemplateColumn column : requisition.getTemplate().getColumnsMap().values()) {
        if (isFalse(column.getIsDisplayed()) || column.getSource() == SourceType.CALCULATED) {
          String field = column.getName();

          try {
            PropertyUtils.setSimpleProperty(lineItem, field, null);
          } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException exp) {
            throw new IllegalArgumentException(
                "Could not set null value for property >" + field + "< in line item", exp
            );
          }
        }
      }
    }
  }

  private ResponseEntity<RequisitionsProcessingStatusDto> buildResponse(
      RequisitionsProcessingStatusDto processingStatus) {
    return new ResponseEntity<>(processingStatus, processingStatus.getRequisitionErrors().isEmpty()
        ? HttpStatus.OK : HttpStatus.BAD_REQUEST);
  }

  private Message.LocalizedMessage localizeMessage(Message message) {
    return message == null ? null : messageService.localize(message);
  }
}
