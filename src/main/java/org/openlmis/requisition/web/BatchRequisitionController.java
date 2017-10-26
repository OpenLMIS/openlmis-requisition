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
import org.openlmis.requisition.dto.BasicOrderableDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionErrorMessage;
import org.openlmis.requisition.dto.RequisitionsProcessingStatusDto;
import org.openlmis.requisition.errorhandling.ValidationFailure;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.utils.Message;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Controller
public class BatchRequisitionController extends BaseRequisitionController {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(
      BatchRequisitionController.class);

  @Autowired
  private MessageService messageService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  /**
   * Attempts to retrieve requisitions with the provided UUIDs.
   */
  @RequestMapping(value = "/requisitions", params = "retrieveAll", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<RequisitionsProcessingStatusDto> retrieveAll(
      @RequestParam(value = "id") List<UUID> uuids) {

    XLOGGER.entry(uuids);
    Profiler profiler = new Profiler("BATCH_RETRIEVE_ALL_REQUISITIONS");
    profiler.setLogger(XLOGGER);

    profiler.start("FIND_ALL_REQUISITIONS_BY_IDS");
    List<Requisition> requisitions = Lists.newArrayList(requisitionRepository.findAll(uuids));

    profiler.start("FIND_ALL_PROGRAMS_FOR_REQUISITIONS");
    Map<UUID, ProgramDto> programs = getUuidProgramDtoMap(requisitions);
    profiler.start("FIND_ALL_FACILITIES_FOR_REQUISITIONS");
    Map<UUID, FacilityDto> facilities = getUuidFacilityDtoMap(requisitions);
    profiler.start("FIND_ALL_ORDERABLES_FOR_REQUISITIONS");
    Map<UUID, OrderableDto> orderables = getUuidOrderableDtoMap(requisitions);

    profiler.start("CHECK_PERM_AND_BUILD_DTO");
    RequisitionsProcessingStatusDto processingStatus = new RequisitionsProcessingStatusDto();
    for (Requisition requisition : requisitions) {
      ValidationResult accessCheck = permissionService.canViewRequisition(requisition);
      if (accessCheck.hasErrors()) {
        processingStatus.addProcessingError(new RequisitionErrorMessage(requisition.getId(),
            localizeMessage(accessCheck.getError().getMessage())));
      } else {
        processingStatus.addProcessedRequisition(
            new ApproveRequisitionDto(
                requisitionDtoBuilder.build(
                    requisition,
                    facilities.get(requisition.getFacilityId()),
                    programs.get(requisition.getProgramId()),
                    orderables)));
      }
    }

    profiler.start("REMOVE_SKIPPED_PRODUCTS");
    processingStatus.removeSkippedProducts();

    profiler.start("BUILD_RESPONSE");
    ResponseEntity<RequisitionsProcessingStatusDto> response = buildResponse(processingStatus);

    profiler.stop().log();
    XLOGGER.exit(processingStatus);
    return response;
  }

  /**
   * Attempts to approve requisitions with the provided UUIDs.
   */
  @RequestMapping(value = "/requisitions", params = "approveAll", method = RequestMethod.POST)
  @ResponseBody
  public ResponseEntity<RequisitionsProcessingStatusDto> approve(
      @RequestParam(value = "id") List<UUID> uuids) {

    XLOGGER.entry(uuids);
    Profiler profiler = new Profiler("BATCH_APPROVE_ALL_REQUISITIONS");
    profiler.setLogger(XLOGGER);

    RequisitionsProcessingStatusDto processingStatus = new RequisitionsProcessingStatusDto();

    profiler.start("GET_USER_ID");
    UUID userId = authenticationHelper.getCurrentUser().getId();

    profiler.start("FIND_VALIDATE_AND_APPROVE_REQUISITIONS");
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

    profiler.start("BUILD_RESPONSE");
    ResponseEntity response = buildResponse(processingStatus);

    profiler.stop().log();
    XLOGGER.exit(processingStatus);
    return response;
  }

  /**
   * Attempts to approve requisitions with the provided UUIDs.
   */
  @RequestMapping(value = "/requisitions", params = "saveAll", method = RequestMethod.PUT)
  @ResponseBody
  public ResponseEntity<RequisitionsProcessingStatusDto> update(
      @RequestBody List<ApproveRequisitionDto> dtos) {

    XLOGGER.entry(dtos);
    Profiler profiler = new Profiler("BATCH_SAVE_ALL_REQUISITIONS");
    profiler.setLogger(XLOGGER);

    RequisitionsProcessingStatusDto processingStatus = new RequisitionsProcessingStatusDto();

    profiler.start("FIND_VALIDATE_AND_SAVE_REQUISITIONS");
    for (ApproveRequisitionDto dto : dtos) {
      profiler.start("VALIDATE_AND_CHECK_SAVE_REQUISITION");
      ValidationResult result = requisitionService.validateCanSaveRequisition(dto.getId());
      if (addValidationErrors(processingStatus, result, dto.getId())) {
        continue;
      }

      profiler.start("FIND_REQUISITION");
      Requisition requisitionToUpdate = requisitionRepository.findOne(dto.getId());

      profiler.start("BUILD_REQUISITION");
      Requisition requisition = buildRequisition(dto, requisitionToUpdate);

      profiler.start("VALIDATE_REQUISITION_TIMESTAMPS");
      result = requisitionVersionValidator.validateRequisitionTimestamps(
          requisition, requisitionToUpdate);
      result.addValidationResult(validateFields(draftValidator, requisition));

      if (addValidationErrors(processingStatus, result, dto.getId())) {
        continue;
      }

      profiler.start("DO_UPDATE");
      RequisitionDto requisitionDto = doUpdate(requisitionToUpdate, requisition);

      profiler.start("ADD_PROCESSED_REQUISITION");
      processingStatus.addProcessedRequisition(new ApproveRequisitionDto(requisitionDto));
    }

    profiler.start("REMOVE_SKIPPED_PRODUCTS");
    processingStatus.removeSkippedProducts();

    profiler.start("BUILD_RESPONSE");
    ResponseEntity response = buildResponse(processingStatus);

    profiler.stop().log();
    XLOGGER.exit(processingStatus);
    return response;
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
          .ifPresent(original -> original.setApprovedQuantity(line.getApprovedQuantity()));
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

  private Map<UUID, FacilityDto> getUuidFacilityDtoMap(List<Requisition> requisitions) {
    Set<UUID> facilityIds = requisitions.stream()
        .map(Requisition::getFacilityId)
        .collect(Collectors.toSet());

    List<FacilityDto> list = facilityReferenceDataService.search(facilityIds);

    Map<UUID, FacilityDto> facilities = new HashMap<>(facilityIds.size());
    for (FacilityDto facility : list) {
      facilities.put(facility.getId(), facility);
    }

    return facilities;
  }

  private Map<UUID, ProgramDto> getUuidProgramDtoMap(List<Requisition> requisitions) {
    Set<UUID> programIds = requisitions.stream()
        .map(Requisition::getProgramId)
        .collect(Collectors.toSet());

    Map<UUID, ProgramDto> programs = new HashMap<>(programIds.size());
    for (UUID programId : programIds) {
      programs.put(programId, programReferenceDataService.findOne(programId));
    }
    return programs;
  }

  private Map<UUID, OrderableDto> getUuidOrderableDtoMap(List<Requisition> requisitions) {
    Set<UUID> orderableIds = requisitions.stream()
        .map(Requisition::getRequisitionLineItems)
        .flatMap(Collection::stream)
        .map(RequisitionLineItem::getOrderableId)
        .collect(Collectors.toSet());

    orderableIds.addAll(requisitions.stream()
        .map(Requisition::getAvailableNonFullSupplyProducts)
        .flatMap(Collection::stream)
        .collect(Collectors.toSet()));

    return orderableReferenceDataService.findByIds(orderableIds)
        .stream()
        .collect(Collectors.toMap(BasicOrderableDto::getId, orderable -> orderable));
  }
}
