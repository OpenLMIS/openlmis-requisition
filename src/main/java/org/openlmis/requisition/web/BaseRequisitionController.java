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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_END_DATE_WRONG;
import static org.springframework.util.CollectionUtils.isEmpty;

import com.google.common.collect.ImmutableList;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionValidationService;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.openlmis.requisition.validate.AbstractRequisitionValidator;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

public abstract class BaseRequisitionController extends BaseController {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(BaseRequisitionController.class);

  protected final Logger logger = LoggerFactory.getLogger(getClass());
  protected static final String REQUISITION = "requisition";

  @Autowired
  protected RequisitionService requisitionService;

  @Autowired
  protected RequisitionRepository requisitionRepository;

  @Autowired
  protected RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  protected PermissionService permissionService;

  @Autowired
  protected AuthenticationHelper authenticationHelper;

  @Autowired
  protected OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  protected BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @Autowired
  protected SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Autowired
  protected RequisitionValidator validator;

  @Autowired
  protected RequisitionVersionValidator requisitionVersionValidator;

  @Autowired
  protected RequisitionStatusProcessor requisitionStatusProcessor;

  @Autowired
  protected StockEventStockManagementService stockEventStockManagementService;

  @Autowired
  protected StockEventBuilder stockEventBuilder;

  @Autowired
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  @Autowired
  private DatePhysicalStockCountCompletedEnabledPredicate
      datePhysicalStockCountCompletedEnabledPredicate;

  @Autowired
  private DateHelper dateHelper;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  protected FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  protected FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @Autowired
  protected MessageService messageService;

  protected ValidationResult validateFields(AbstractRequisitionValidator validator,
                                            Requisition requisition) {
    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      logger.warn("Validation for requisition failed: {}", getErrors(bindingResult));
      return ValidationResult.fieldErrors(getErrors(bindingResult));
    }

    return ValidationResult.success();
  }

  protected RequisitionDto doUpdate(Requisition requisitionToUpdate, Requisition requisition) {
    updateRequisition(requisitionToUpdate, requisition);
    requisitionToUpdate = requisitionRepository.save(requisitionToUpdate);

    logger.debug("Requisition with id {} saved", requisitionToUpdate.getId());
    return requisitionDtoBuilder.build(requisitionToUpdate);
  }

  private void updateRequisition(Requisition requisitionToUpdate,
                                 Requisition requisition) {
    requisitionToUpdate.updateFrom(requisition,
        orderableReferenceDataService.findByIds(getLineItemOrderableIds(requisition)),
        datePhysicalStockCountCompletedEnabledPredicate.exec(requisitionToUpdate.getProgramId()));
  }

  protected BasicRequisitionDto doApprove(Requisition requisition, UserDto user) {
    XLOGGER.entry(requisition, user);
    Profiler profiler = new Profiler("DO_APPROVE");
    profiler.setLogger(XLOGGER);

    profiler.start("CHECK_IF_PERIOD_IS_VALID");
    checkIfPeriodIsValid(requisition);

    profiler.start("GET_SUPERVISORY_NODE");
    SupervisoryNodeDto supervisoryNodeDto =
        supervisoryNodeReferenceDataService.findOne(requisition.getSupervisoryNodeId());

    SupervisoryNodeDto parentNode = null;
    UUID parentNodeId = null;

    profiler.start("SET_PARENT_NODE_ID");
    if (supervisoryNodeDto != null) {
      parentNode = supervisoryNodeDto.getParentNode();
    }

    if (parentNode != null) {
      parentNodeId = parentNode.getId();
    }

    List<SupplyLineDto> supplyLines = supplyLineReferenceDataService.search(
        requisition.getProgramId(), requisition.getSupervisoryNodeId());

    requisitionService.doApprove(parentNodeId, user.getId(),
        getLineItemOrderableIds(requisition), requisition, supplyLines);

    if (requisition.getStatus().isApproved()) {
      profiler.start("BUILD_STOCK_EVENT_FROM_REQUISITION");
      StockEventDto stockEventDto = stockEventBuilder.fromRequisition(requisition);

      profiler.start("SUBMIT_STOCK_EVENT");
      stockEventStockManagementService.submit(stockEventDto);

      if (!isEmpty(supplyLines)) {
        profiler.start("RETRIEVE_SUPPLYING_FACILITY");
        FacilityDto facility = facilityReferenceDataService
            .findOne(supplyLines.get(0).getSupplyingFacility());

        profiler.start("FIND_SUPPORTED_PROGRAM_ENTRY");
        SupportedProgramDto supportedProgram = facilitySupportsProgramHelper
            .getSupportedProgram(facility, requisition.getProgramId());

        if (supportedProgram.isSupportLocallyFulfilled()) {
          profiler.start("CONVERT_TO_ORDER");
          ConvertToOrderDto entry = new ConvertToOrderDto(requisition.getId(), facility.getId());
          requisitionService.convertToOrder(ImmutableList.of(entry), user);
        }
      }
    }

    profiler.start("PROCESS_STATUS_CHANGE");
    requisitionStatusProcessor.statusChange(requisition);

    logger.debug("Requisition with id {} approved", requisition.getId());
    profiler.start("BUILD_REQUISITION_DTO");
    BasicRequisitionDto basicRequisitionDto = basicRequisitionDtoBuilder.build(requisition);

    profiler.stop().log();
    XLOGGER.exit(basicRequisitionDto);
    return basicRequisitionDto;
  }

  protected Set<UUID> getLineItemOrderableIds(Requisition requisition) {
    return requisition.getRequisitionLineItems().stream().map(
        RequisitionLineItem::getOrderableId).collect(Collectors.toSet());
  }

  protected void checkIfPeriodIsValid(Requisition requisition) {
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

  protected ValidationResult validateRequisitionCanBeUpdated(Requisition requisitionToUpdate,
                                                             Requisition requisition) {
    return requisitionToUpdate.validateCanBeUpdated(new RequisitionValidationService(
        requisition, requisitionToUpdate,
        dateHelper.getCurrentDateWithSystemZone(),
        datePhysicalStockCountCompletedEnabledPredicate.exec(requisitionToUpdate.getProgramId())));
  }
}
