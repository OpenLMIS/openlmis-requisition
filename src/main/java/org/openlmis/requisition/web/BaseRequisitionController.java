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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_END_DATE_WRONG;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.openlmis.requisition.validate.AbstractRequisitionValidator;
import org.openlmis.requisition.validate.DraftRequisitionValidator;
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
  protected StatusMessageRepository statusMessageRepository;

  @Autowired
  protected BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @Autowired
  protected SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Autowired
  protected RequisitionValidator validator;

  @Autowired
  protected DraftRequisitionValidator draftValidator;

  @Autowired
  protected RequisitionVersionValidator requisitionVersionValidator;

  @Autowired
  protected RequisitionStatusProcessor requisitionStatusProcessor;

  @Autowired
  protected StockEventStockManagementService stockEventStockManagementService;

  @Autowired
  protected StockEventBuilder stockEventBuilder;

  @Autowired
  private DatePhysicalStockCountCompletedEnabledPredicate predicate;

  @Autowired
  private DateHelper dateHelper;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

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
    requisitionToUpdate.updateFrom(requisition, orderableReferenceDataService.findByIds(
            getLineItemOrderableIds(requisition)),
        predicate.exec(requisitionToUpdate.getProgramId()));

    requisitionToUpdate = requisitionRepository.save(requisitionToUpdate);

    logger.debug("Saved requisition with id: " + requisitionToUpdate.getId());
    return requisitionDtoBuilder.build(requisitionToUpdate);
  }

  protected BasicRequisitionDto doApprove(Requisition requisition, UUID userId) {
    XLOGGER.entry(requisition, userId);
    Profiler profiler = new Profiler("DO_APPROVE");
    profiler.setLogger(XLOGGER);

    profiler.start("CHECK_IF_PERIOD_IS_VALID");
    checkIfPeriodIsValid(requisition);

    profiler.start("GET_SUPERVISORY_NODE");
    SupervisoryNodeDto supervisoryNodeDto =
        supervisoryNodeReferenceDataService.findOne(requisition.getSupervisoryNodeId());

    profiler.start("SET_PARENT_NODE_ID");
    UUID parentNodeId = null;
    if (supervisoryNodeDto != null) {
      SupervisoryNodeDto parentNode = supervisoryNodeDto.getParentNode();
      if (parentNode != null) {
        parentNodeId = parentNode.getId();
      }
    }

    if (parentNodeId == null) {
      profiler.start("SUBMIT_STOCK_EVENT");
      stockEventStockManagementService.submit(
          stockEventBuilder.fromRequisition(requisition)
      );
    }

    profiler.start("GET_ORDERABLES");
    Collection<OrderableDto> orderables = orderableReferenceDataService.findByIds(
        getLineItemOrderableIds(requisition));

    profiler.start("APPROVE_REQUISITION_ENTITY");
    requisition.approve(parentNodeId, orderables, userId);

    profiler.start("SAVE_STATUS_MESSAGE");
    saveStatusMessage(requisition);

    profiler.start("SAVE_REQUISITION_TO_REPO");
    requisitionRepository.save(requisition);

    profiler.start("PROCESS_STATUS_CHANGE");
    requisitionStatusProcessor.statusChange(requisition);

    logger.debug("Requisition with id {} approved", requisition.getId());
    profiler.start("BUILD_REQUISITION_DTO");
    BasicRequisitionDto basicRequisitionDto = basicRequisitionDtoBuilder.build(requisition);

    profiler.stop().log();
    XLOGGER.exit();
    return basicRequisitionDto;
  }

  protected void saveStatusMessage(Requisition requisition) {
    if (isNotBlank(requisition.getDraftStatusMessage())) {
      StatusMessage newStatusMessage = StatusMessage.newStatusMessage(requisition,
          authenticationHelper.getCurrentUser().getId(),
          authenticationHelper.getCurrentUser().getFirstName(),
          authenticationHelper.getCurrentUser().getLastName(),
          requisition.getDraftStatusMessage());
      statusMessageRepository.save(newStatusMessage);
      requisition.setDraftStatusMessage("");
    }
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
}
