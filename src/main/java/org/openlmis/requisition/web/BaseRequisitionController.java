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

import static org.apache.commons.lang3.BooleanUtils.isNotTrue;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FACILITY_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_END_DATE_WRONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PROGRAM_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.springframework.util.CollectionUtils.isEmpty;

import com.google.common.collect.ImmutableList;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionValidationService;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;

@SuppressWarnings("PMD.TooManyMethods")
public abstract class BaseRequisitionController extends BaseController {
  private final XLogger extLogger = XLoggerFactory.getXLogger(getClass());
  final Logger logger = LoggerFactory.getLogger(getClass());

  @Autowired
  RequisitionService requisitionService;

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  PermissionService permissionService;

  @Autowired
  AuthenticationHelper authenticationHelper;

  @Autowired
  OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @Autowired
  RequisitionVersionValidator requisitionVersionValidator;

  @Autowired
  SupplyLineReferenceDataService supplyLineReferenceDataService;

  @Autowired
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @Autowired
  private StockEventStockManagementService stockEventStockManagementService;

  @Autowired
  private StockEventBuilder stockEventBuilder;

  @Autowired
  private DatePhysicalStockCountCompletedEnabledPredicate
      datePhysicalStockCountCompletedEnabledPredicate;

  @Autowired
  private DateHelper dateHelper;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  RequisitionDto doUpdate(Requisition requisitionToUpdate, Requisition requisition) {
    Profiler profiler = getProfiler("UPDATE_REQUISITION");

    FacilityDto facility = findFacility(requisitionToUpdate.getFacilityId(), profiler);
    ProgramDto program = findProgram(requisitionToUpdate.getProgramId(), profiler);
    Map<UUID, OrderableDto> orderables = findOrderables(
        profiler, requisitionToUpdate::getAllOrderableIds
    );

    RequisitionDto dto = doUpdate(
        requisitionToUpdate, requisition, orderables, facility, program, profiler
    );

    stopProfiler(profiler, dto);
    return dto;
  }

  RequisitionDto doUpdate(Requisition toUpdate, Requisition requisition,
      Map<UUID, OrderableDto> orderables, FacilityDto facility, ProgramDto program,
      Profiler profiler) {
    profiler.start("UPDATE");
    toUpdate.updateFrom(requisition, orderables,
        datePhysicalStockCountCompletedEnabledPredicate.exec(program));

    profiler.start("SAVE");
    toUpdate = requisitionRepository.save(toUpdate);
    logger.debug("Requisition with id {} saved", toUpdate.getId());

    return buildDto(profiler, toUpdate, orderables, facility, program);
  }

  void doApprove(Requisition requisition, UserDto user,
      SupervisoryNodeDto supervisoryNode, Map<UUID, OrderableDto> orderables,
      List<SupplyLineDto> supplyLines) {
    Profiler profiler = getProfiler("DO_APPROVE_REQUISITION", requisition, user);
    checkIfPeriodIsValid(requisition, profiler);

    SupervisoryNodeDto parentNode = null;
    UUID parentNodeId = null;

    profiler.start("SET_PARENT_NODE_ID");
    if (supervisoryNode != null) {
      parentNode = supervisoryNode.getParentNode();
    }

    if (parentNode != null) {
      parentNodeId = parentNode.getId();
    }

    profiler.start("DO_APPROVE");
    requisitionService.doApprove(parentNodeId, user, orderables, requisition, supplyLines);

    if (requisition.getStatus().isApproved() && !isEmpty(supplyLines)) {
      profiler.start("RETRIEVE_SUPPLYING_FACILITY");
      FacilityDto facility = facilityReferenceDataService
          .findOne(supplyLines.get(0).getSupplyingFacility());

      profiler.start("FIND_SUPPORTED_PROGRAM_ENTRY");
      SupportedProgramDto supportedProgram = facilitySupportsProgramHelper
          .getSupportedProgram(facility, requisition.getProgramId());

      if (supportedProgram != null && supportedProgram.isSupportLocallyFulfilled()) {
        profiler.start("CONVERT_TO_ORDER");
        ConvertToOrderDto entry = new ConvertToOrderDto(requisition.getId(), facility.getId());
        requisitionService.convertToOrder(ImmutableList.of(entry), user);
      }
    }

    callStatusChangeProcessor(profiler, requisition);

    logger.debug("Requisition with id {} approved", requisition.getId());
    stopProfiler(profiler);
  }

  void submitStockEvent(Requisition requisition, UUID currentUserId) {
    Profiler profiler = getProfiler("SUBMIT_STOCK_EVENT", requisition, currentUserId);
    if (requisition.getStatus().isApproved()
        && isNotTrue(requisition.getEmergency())) {
      profiler.start("BUILD_STOCK_EVENT_FROM_REQUISITION");
      StockEventDto stockEventDto = stockEventBuilder.fromRequisition(requisition, currentUserId);

      profiler.start("SUBMIT_STOCK_EVENT");
      stockEventStockManagementService.submit(stockEventDto);
      stopProfiler(profiler, stockEventDto);
    }
  }

  Set<UUID> getLineItemOrderableIds(Requisition requisition) {
    return requisition.getRequisitionLineItems().stream().map(
        RequisitionLineItem::getOrderableId).collect(Collectors.toSet());
  }

  Set<UUID> getLineItemOrderableIds(Collection<Requisition> requisitions) {
    return requisitions.stream()
        .flatMap(r -> r.getRequisitionLineItems().stream())
        .map(RequisitionLineItem::getOrderableId)
        .collect(Collectors.toSet());
  }

  void checkIfPeriodIsValid(Requisition requisition, Profiler profiler) {
    profiler.start("CHECK_IF_PERIOD_IS_VALID");

    if (requisition.getEmergency() != null && !requisition.getEmergency()) {
      LocalDate endDate = periodReferenceDataService
          .findOne(requisition.getProcessingPeriodId())
          .getEndDate();
      if (dateHelper.isDateAfterNow(endDate)) {
        throw new ValidationMessageException(new Message(
            ERROR_PERIOD_END_DATE_WRONG, DateTimeFormatter.ISO_DATE.format(endDate)));
      }
    }
  }

  ValidationResult validateRequisitionCanBeUpdated(Requisition requisitionToUpdate,
      Requisition requisition) {
    return requisitionToUpdate.validateCanBeUpdated(new RequisitionValidationService(
        requisition, requisitionToUpdate,
        dateHelper.getCurrentDateWithSystemZone(),
        datePhysicalStockCountCompletedEnabledPredicate.exec(requisitionToUpdate.getProgramId())));
  }

  ValidationResult validateRequisitionCanBeUpdated(Requisition requisitionToUpdate,
      Requisition requisition, ProgramDto program) {
    return requisitionToUpdate.validateCanBeUpdated(new RequisitionValidationService(
        requisition, requisitionToUpdate,
        dateHelper.getCurrentDateWithSystemZone(),
        datePhysicalStockCountCompletedEnabledPredicate.exec(program)));
  }

  ValidationResult getValidationResultForStatusChange(Requisition requisition) {
    return requisition.validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),
        datePhysicalStockCountCompletedEnabledPredicate.exec(requisition.getProgramId()));
  }

  Profiler getProfiler(String name, Object... entryArgs) {
    extLogger.entry(entryArgs);

    Profiler profiler = new Profiler(name);
    profiler.setLogger(extLogger);

    return profiler;
  }

  void stopProfiler(Profiler profiler, Object... exitArgs) {
    profiler.stop().log();
    extLogger.exit(exitArgs);
  }

  Requisition findRequisition(UUID requisitionId, Profiler profiler) {
    profiler.start("GET_REQUISITION_BY_ID");
    return findResource(
        profiler, requisitionId, requisitionRepository::findOne, ERROR_REQUISITION_NOT_FOUND
    );
  }

  FacilityDto findFacility(UUID facilityId, Profiler profiler) {
    profiler.start("GET_FACILITY");
    return findResource(
        profiler, facilityId, facilityReferenceDataService::findOne, ERROR_FACILITY_NOT_FOUND
    );
  }

  ProgramDto findProgram(UUID programId, Profiler profiler) {
    profiler.start("GET_PROGRAM");
    return findResource(
        profiler, programId, programReferenceDataService::findOne, ERROR_PROGRAM_NOT_FOUND
    );
  }

  private <R> R findResource(Profiler profiler, UUID id, Function<UUID, R> finder,
      String errorMessage) {
    return Optional
        .ofNullable(finder.apply(id))
        .orElseThrow(() -> {
          stopProfiler(profiler);
          return new ContentNotFoundMessageException(errorMessage, id);
        });
  }

  Map<UUID, OrderableDto> findOrderables(Profiler profiler, Supplier<Set<UUID>> supplier) {
    profiler.start("GET_ORDERABLES");
    return orderableReferenceDataService
        .findByIds(supplier.get())
        .stream()
        .collect(Collectors.toMap(OrderableDto::getId, Function.identity()));
  }

  void checkPermission(Profiler profiler, Supplier<ValidationResult> supplier) {
    profiler.start("CHECK_PERMISSION");
    supplier.get().throwExceptionIfHasErrors();
  }

  void validateForStatusChange(Requisition requisition, Profiler profiler) {
    profiler.start("VALIDATE_CAN_CHANGE_STATUS");
    getValidationResultForStatusChange(requisition)
        .throwExceptionIfHasErrors();
  }

  UserDto getCurrentUser(Profiler profiler) {
    profiler.start("GET_CURRENT_USER");
    return authenticationHelper.getCurrentUser();
  }

  void callStatusChangeProcessor(Profiler profiler, Requisition requisition) {
    profiler.start("CALL_STATUS_CHANGE_PROCESSOR");
    assignInitialSupervisoryNode(requisition);
    requisitionStatusProcessor.statusChange(requisition);
  }

  private void assignInitialSupervisoryNode(Requisition requisition) {
    if (requisition.isApprovable()
        && requisition.getSupervisoryNodeId() == null) {
      UUID supervisoryNode = supervisoryNodeReferenceDataService.findSupervisoryNode(
          requisition.getProgramId(), requisition.getFacilityId()).getId();
      requisition.setSupervisoryNodeId(supervisoryNode);
    }
  }

  RequisitionDto buildDto(Profiler profiler, Requisition requisition,
      Map<UUID, OrderableDto> orderables, FacilityDto facility, ProgramDto program) {
    profiler.start("BUILD_REQUISITION_DTO");
    return requisitionDtoBuilder.build(requisition, orderables, facility, program);
  }

  BasicRequisitionDto buildBasicDto(Profiler profiler, Requisition requisition) {
    profiler.start("BUILD_BASIC_REQUISITION_DTO");
    return basicRequisitionDtoBuilder.build(requisition);
  }
}
