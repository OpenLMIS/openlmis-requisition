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

import static java.util.concurrent.CompletableFuture.runAsync;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.apache.commons.lang3.BooleanUtils.isFalse;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;

import com.google.common.collect.Lists;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.dto.ApproveRequisitionDto;
import org.openlmis.requisition.dto.ApproveRequisitionLineItemDto;
import org.openlmis.requisition.dto.BasicOrderableDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionErrorMessage;
import org.openlmis.requisition.dto.RequisitionsProcessingStatusDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationFailure;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
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

@Controller
@SuppressWarnings("PMD.TooManyMethods")
public class BatchRequisitionController extends BaseRequisitionController {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(
      BatchRequisitionController.class);

  @Autowired
  private MessageService messageService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

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

    profiler.start("FIND_ALL_FACILITIES_FOR_REQUISITIONS");
    Map<UUID, FacilityDto> facilities = getUuidFacilityDtoMap(requisitions);
    profiler.start("FIND_ALL_ORDERABLES_FOR_REQUISITIONS");
    Map<UUID, OrderableDto> orderables = getUuidOrderableDtoMap(requisitions);
    profiler.start("FIND_ALL_PERIODS_FOR_REQUISITIONS");
    Map<UUID, ProcessingPeriodDto> periods = getUuidPeriodDtoMap(requisitions);

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
                requisitionDtoBuilder.buildBatch(
                    requisition,
                    facilities.get(requisition.getFacilityId()),
                    orderables,
                    periods.get(requisition.getProcessingPeriodId()))));
      }
    }

    profiler.start("REMOVE_SKIPPED_PRODUCTS");
    processingStatus.removeSkippedProducts();

    ResponseEntity<RequisitionsProcessingStatusDto> response = buildResponse(processingStatus,
        profiler);

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

    profiler.start("GET_USER");
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("FIND_REQUISITIONS");
    List<Requisition> requisitions = requisitionRepository.readDistinctByIdIn(uuids);

    profiler.start("GET_USER_PERMISSION_STRINGS");
    List<String> permissionStrings = userReferenceDataService.getPermissionStrings(user.getId());

    Map<UUID, SupervisoryNodeDto> supervisoryNodeMap = findSupervisoryNodes(requisitions, profiler);
    Map<UUID, OrderableDto> orderables = findOrderables(
        profiler, () -> getLineItemOrderableIds(requisitions)
    );
    Map<Pair<UUID, UUID>, List<SupplyLineDto>> supplyLinesMap =
        findSupplyLines(requisitions, profiler);

    profiler.start("FIND_ALL_FACILITIES_FOR_REQUISITIONS");
    Map<UUID, FacilityDto> facilities = getUuidFacilityDtoMap(requisitions);

    profiler.start("FIND_ALL_PERIODS_FOR_REQUISITIONS");
    Map<UUID, ProcessingPeriodDto> periods = getUuidPeriodDtoMap(requisitions);

    profiler.start("VALIDATE_AND_APPROVE");
    for (Requisition requisition : requisitions) {
      SupervisoryNodeDto supervisoryNode = supervisoryNodeMap
          .get(requisition.getSupervisoryNodeId());
      List<SupplyLineDto> supplyLines = supplyLinesMap
          .get(Pair.of(requisition.getProgramId(), requisition.getSupervisoryNodeId()));
      validateAndApprove(requisition, processingStatus, user, supervisoryNode, permissionStrings,
          orderables, supplyLines, facilities, periods);
    }

    submitStockEvent(profiler, user, requisitions);

    ResponseEntity<RequisitionsProcessingStatusDto> response =
        buildResponse(processingStatus, profiler);

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
      if (!addValidationErrors(processingStatus, result, dto.getId())) {
        profiler.start("FIND_REQUISITION");
        Requisition requisitionToUpdate = requisitionRepository.findOne(dto.getId());

        profiler.start("BUILD_REQUISITION");
        Requisition requisition = buildRequisition(dto, requisitionToUpdate);

        profiler.start("VALIDATE_REQUISITION_TIMESTAMPS");
        result = requisitionVersionValidator.validateRequisitionTimestamps(
            requisition, requisitionToUpdate);
        result
            .addValidationResult(validateRequisitionCanBeUpdated(requisitionToUpdate, requisition));

        if (!addValidationErrors(processingStatus, result, dto.getId())) {
          profiler.start("DO_UPDATE");
          RequisitionDto requisitionDto = doUpdate(requisitionToUpdate, requisition);

          profiler.start("ADD_PROCESSED_REQUISITION");
          processingStatus.addProcessedRequisition(new ApproveRequisitionDto(requisitionDto));
        }
      }
    }

    profiler.start("REMOVE_SKIPPED_PRODUCTS");
    processingStatus.removeSkippedProducts();

    ResponseEntity<RequisitionsProcessingStatusDto> response =
        buildResponse(processingStatus, profiler);

    profiler.stop().log();
    XLOGGER.exit(processingStatus);
    return response;
  }

  private Map<UUID, SupervisoryNodeDto> findSupervisoryNodes(List<Requisition> requisitions,
      Profiler profiler) {
    profiler.start("FIND_SUPERVISORY_NODES");
    List<UUID> supervisoryNodeIds = requisitions.stream()
        .map(Requisition::getSupervisoryNodeId)
        .collect(toList());

    return supervisoryNodeService
        .findByIds(supervisoryNodeIds)
        .stream()
        .collect(toMap(SupervisoryNodeDto::getId, supervisoryNode -> supervisoryNode));
  }

  private Map<Pair<UUID, UUID>, List<SupplyLineDto>> findSupplyLines(
      List<Requisition> requisitions, Profiler profiler) {
    profiler.start("GET_SUPPLY_LINES");
    Set<Pair<UUID, UUID>> programsFacilities = new HashSet<>();
    requisitions
        .forEach(r -> programsFacilities.add(Pair.of(r.getProgramId(), r.getSupervisoryNodeId())));

    Map<Pair<UUID, UUID>, List<SupplyLineDto>> supplyLinesMap = new HashMap<>();
    for (Pair<UUID, UUID> pair : programsFacilities) {
      supplyLinesMap.put(pair,
          supplyLineReferenceDataService.search(pair.getLeft(), pair.getRight()));
    }
    return supplyLinesMap;
  }

  private void validateAndApprove(Requisition requisition,
      RequisitionsProcessingStatusDto processingStatus, UserDto user,
      SupervisoryNodeDto supervisoryNode, List<String> permissionStrings,
      Map<UUID, OrderableDto> orderables, List<SupplyLineDto> supplyLines,
      Map<UUID, FacilityDto> facilities, Map<UUID, ProcessingPeriodDto> periods) {
    Profiler profiler = getProfiler("VALIDATE_AND_APPROVE_REQUISITION");
    profiler.start("VALIDATE_CAN_APPROVE");
    ValidationResult validationResult = validateCanApproveRequisition(
        requisition, permissionStrings);
    if (!addValidationErrors(processingStatus, validationResult, requisition.getId())) {
      profiler.start("VALIDATE_FOR_STATUS_CHANGE");
      validationResult = getValidationResultForStatusChange(requisition);
      if (!addValidationErrors(processingStatus, validationResult, requisition.getId())) {
        profiler.start("DO_APPROVE");
        doApprove(requisition, user, supervisoryNode, orderables, supplyLines);
        profiler.start("BUILD_DTO_AND_ADD_TO_PROCESSING_STATUS");
        processingStatus.addProcessedRequisition(
            new ApproveRequisitionDto(requisitionDtoBuilder
                .buildBatch(requisition, facilities.get(requisition.getFacilityId()), orderables,
                    periods.get(requisition.getProcessingPeriodId()))));
      }
    }
    stopProfiler(profiler);
  }

  private ValidationResult validateCanApproveRequisition(Requisition requisition,
      List<String> permissionStrings) {
    String permission = String.format("%s|%s|%s", PermissionService.REQUISITION_APPROVE,
        requisition.getFacilityId(), requisition.getProgramId());

    if (!permissionStrings.contains(permission)) {
      return ValidationResult
          .noPermission(ERROR_NO_FOLLOWING_PERMISSION, PermissionService.REQUISITION_APPROVE);
    }

    if (!requisition.isApprovable()) {
      return ValidationResult.failedValidation(MessageKeys
          .ERROR_REQUISITION_MUST_BE_AUTHORIZED, requisition.getId());
    }

    return ValidationResult.success();
  }

  private void submitStockEvent(Profiler profiler, UserDto user, List<Requisition> requisitions) {
    profiler.start("SEND_STOCK_EVENT");
    ExecutorService executor = Executors.newFixedThreadPool(requisitions.size());
    List<CompletableFuture<Void>> futures = Lists.newArrayList();
    try {
      for (Requisition requisition : requisitions) {
        CompletableFuture<Void> future = runAsync(
            () -> submitStockEvent(requisition, user.getId()), executor);
        futures.add(future);
      }
    } finally {
      profiler.start("JOIN_RESULTS");
      futures.forEach(CompletableFuture::join);
    }
  }

  private Requisition buildRequisition(ApproveRequisitionDto dto, Requisition requisitionToUpdate) {
    Map<UUID, OrderableDto> orderables = orderableReferenceDataService
        .findByIds(requisitionToUpdate.getAllOrderableIds())
        .stream()
        .collect(toMap(OrderableDto::getId, Function.identity()));

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDtoBuilder.build(requisitionToUpdate),
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        requisitionToUpdate.getStatus(), orderables);
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
    setNullForCalculatedFields(requisition);
    return requisition;
  }

  private boolean addValidationErrors(RequisitionsProcessingStatusDto processingStatus,
      ValidationResult validationResult, UUID id) {
    if (validationResult.hasErrors()) {
      for (ValidationFailure failure : validationResult.gerErrors()) {
        Map<String, Message.LocalizedMessage> localizedErrors = new HashMap<>();
        failure.getFieldErrors()
            .forEach((field, message) -> localizedErrors.put(field, localizeMessage(message)));

        processingStatus.addProcessingError(
            new RequisitionErrorMessage(id, localizeMessage(failure.getMessage()),
                localizedErrors));
      }

      return true;
    }

    return false;
  }

  private void setNullForCalculatedFields(Requisition requisition) {
    for (RequisitionLineItem lineItem : requisition.getRequisitionLineItems()) {
      for (RequisitionTemplateColumn column : requisition.getTemplate().viewColumns().values()) {
        if (isFalse(column.getIsDisplayed()) || column.getSource() == SourceType.CALCULATED) {
          setNullForField(lineItem, column);
        }
      }
    }
  }

  private void setNullForField(RequisitionLineItem lineItem, RequisitionTemplateColumn column) {
    String field = column.getName();

    try {
      PropertyUtils.setSimpleProperty(lineItem, field, null);
    } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException exp) {
      throw new IllegalArgumentException(
          "Could not set null value for property >" + field + "< in line item", exp
      );
    }
  }

  private ResponseEntity<RequisitionsProcessingStatusDto> buildResponse(
      RequisitionsProcessingStatusDto processingStatus, Profiler profiler) {
    profiler.start("BUILD_RESPONSE");
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

  private Map<UUID, ProcessingPeriodDto> getUuidPeriodDtoMap(List<Requisition> requisitions) {
    Set<UUID> periodIds = requisitions.stream()
        .map(Requisition::getProcessingPeriodId)
        .collect(Collectors.toSet());

    Map<UUID, ProcessingPeriodDto> periods = new HashMap<>(periodIds.size());
    for (UUID periodId : periodIds) {
      periods.put(periodId, periodService.getPeriod(periodId));
    }

    return periods;
  }

  private Map<UUID, OrderableDto> getUuidOrderableDtoMap(List<Requisition> requisitions) {
    Set<UUID> orderableIds = requisitions.stream()
        .map(Requisition::getRequisitionLineItems)
        .flatMap(Collection::stream)
        .map(RequisitionLineItem::getOrderableId)
        .collect(Collectors.toSet());

    return orderableReferenceDataService.findByIds(orderableIds)
        .stream()
        .collect(toMap(BasicOrderableDto::getId, orderable -> orderable));
  }
}
