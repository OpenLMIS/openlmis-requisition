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

package org.openlmis.requisition.service;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DELETE_FAILED_WRONG_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_HAVE_SUPPLYING_FACILITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PROGRAM_DOES_NOT_ALLOW_SKIP;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PROGRAM_ID_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_MUST_BE_APPROVED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_TEMPLATE_NOT_DEFINED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_TEMPLATE_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SKIP_FAILED_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SKIP_FAILED_WRONG_STATUS;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.web.OrderDtoBuilder;
import org.openlmis.requisition.web.PermissionMessageException;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.ConvertHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.Pagination;
import org.openlmis.utils.RequisitionDtoComparator;
import org.openlmis.utils.RightName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
// TODO: split this up in OLMIS-1102
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private StatusMessageRepository statusMessageRepository;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private ApprovedProductReferenceDataService approvedProductReferenceDataService;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  private OrderFulfillmentService orderFulfillmentService;

  @Autowired
  private ConvertHelper convertHelper;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private OrderDtoBuilder orderDtoBuilder;

  @Autowired
  private UserRoleAssignmentsReferenceDataService userRoleAssignmentsReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  @Autowired
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @Autowired
  private ProofOfDeliveryService proofOfDeliveryService;

  /**
   * Initiated given requisition if possible.
   *
   * @param programId         UUID of Program.
   * @param facilityId        UUID of Facility.
   * @param emergency         Emergency status.
   * @param suggestedPeriodId Period for requisition.
   * @return Initiated requisition.
   */
  public Requisition initiate(UUID programId, UUID facilityId, UUID suggestedPeriodId,
                              boolean emergency) {
    Requisition requisition = RequisitionBuilder.newRequisition(
        facilityId, programId, emergency);
    requisition.setStatus(RequisitionStatus.INITIATED);

    ProcessingPeriodDto period = periodService
        .findPeriod(programId, facilityId, suggestedPeriodId, emergency);

    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    Collection<ApprovedProductDto> approvedProducts =
        approvedProductReferenceDataService.getApprovedProducts(
            facilityId, programId, true);

    RequisitionTemplate requisitionTemplate = findRequisitionTemplate(programId);

    int numberOfPreviousPeriodsToAverage;
    List<Requisition> previousRequisitions;
    // numberOfPeriodsToAverage is always >= 2 or null
    if (requisitionTemplate.getNumberOfPeriodsToAverage() == null) {
      numberOfPreviousPeriodsToAverage = 0;
      previousRequisitions = getRecentRequisitions(requisition, 1);
    } else {
      numberOfPreviousPeriodsToAverage = requisitionTemplate.getNumberOfPeriodsToAverage() - 1;
      previousRequisitions =
          getRecentRequisitions(requisition, numberOfPreviousPeriodsToAverage);
    }

    if (numberOfPreviousPeriodsToAverage > previousRequisitions.size()) {
      numberOfPreviousPeriodsToAverage = previousRequisitions.size();
    }

    ProofOfDeliveryDto pod = getProofOfDeliveryDto(emergency, requisition);

    requisition.initiate(requisitionTemplate, approvedProducts, previousRequisitions,
        numberOfPreviousPeriodsToAverage, pod, authenticationHelper.getCurrentUser().getId());

    requisition.setAvailableNonFullSupplyProducts(approvedProductReferenceDataService
        .getApprovedProducts(facilityId, programId, false)
        .stream()
        .map(ap -> ap.getOrderable().getId())
        .collect(Collectors.toSet()));

    requisitionRepository.save(requisition);
    return requisition;
  }

  private ProofOfDeliveryDto getProofOfDeliveryDto(boolean emergency, Requisition requisition) {
    List<Requisition> previous = getRecentRequisitions(requisition, 1);
    ProofOfDeliveryDto pod = null;

    if (!emergency && !isEmpty(previous)) {
      pod = proofOfDeliveryService.get(previous.get(0));
    }

    return pod;
  }

  private RequisitionTemplate findRequisitionTemplate(UUID programId) {
    if (null == programId) {
      throw new ValidationMessageException(new Message(ERROR_PROGRAM_ID_CANNOT_BE_NULL));
    }

    RequisitionTemplate template =
        requisitionTemplateService.getTemplateForProgram(programId);

    if (null == template) {
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_TEMPLATE_NOT_FOUND));
    }

    if (!template.hasColumnsDefined()) {
      throw new ValidationMessageException(new Message(ERROR_REQUISITION_TEMPLATE_NOT_DEFINED));
    } else {
      return template;
    }
  }

  /**
   * Delete given Requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be deleted.
   */
  public void delete(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
          requisitionId));
    } else if (!requisition.isPreAuthorize()) {
      throw new ValidationMessageException(new Message(ERROR_DELETE_FAILED_WRONG_STATUS));
    } else {
      statusMessageRepository.delete(statusMessageRepository.findByRequisitionId(requisitionId));
      requisitionRepository.delete(requisition);
      LOGGER.debug("Requisition deleted");
    }
  }

  /**
   * Skip given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be skipped.
   * @return Skipped Requisition.
   */
  public Requisition skip(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
          requisitionId));
    } else {
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
      if (requisition.getStatus() != RequisitionStatus.INITIATED) {
        throw new ValidationMessageException(new Message(ERROR_SKIP_FAILED_WRONG_STATUS));
      } else if (!program.getPeriodsSkippable()) {
        throw new ValidationMessageException(new Message(ERROR_PROGRAM_DOES_NOT_ALLOW_SKIP));
      } else if (requisition.getEmergency()) {
        throw new ValidationMessageException(new Message(ERROR_SKIP_FAILED_EMERGENCY));
      } else {
        LOGGER.debug("Requisition skipped");

        for (RequisitionLineItem item : requisition.getRequisitionLineItems()) {
          item.skipLineItem(requisition.getTemplate());
        }
        requisition.setStatus(RequisitionStatus.SKIPPED);
        return requisitionRepository.save(requisition);
      }
    }
  }

  /**
   * Reject given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be rejected.
   */
  public Requisition reject(UUID requisitionId) {

    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
          requisitionId));
    } else if (requisition.isApprovable()) {
      UUID userId = authenticationHelper.getCurrentUser().getId();
      checkIfCanApproveRequisition(requisition.getProgramId(),
          requisition.getSupervisoryNodeId(), userId);

      LOGGER.debug("Requisition rejected: {}", requisitionId);
      requisition.reject(orderableReferenceDataService.findAll(), userId);
      return requisitionRepository.save(requisition);
    } else {
      throw new ValidationMessageException(new Message(
          ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL, requisitionId));
    }
  }
  
  /**
   * Finds requisitions matching all of the provided parameters.
   */
  public List<Requisition> searchRequisitions(UUID facility, UUID program,
                                              ZonedDateTime initiatedDateFrom,
                                              ZonedDateTime initiatedDateTo,
                                              UUID processingPeriod,
                                              UUID supervisoryNode,
                                              Set<RequisitionStatus> requisitionStatuses,
                                              Boolean emergency) {
    return requisitionRepository.searchRequisitions(facility, program, initiatedDateFrom,
        initiatedDateTo, processingPeriod, supervisoryNode, requisitionStatuses, emergency);
  }

  /**
   * Finds requisitions matching all of the provided parameters.
   */
  public List<Requisition> searchRequisitions(UUID facility,
                                              UUID program,
                                              UUID processingPeriod) {
    return requisitionRepository.searchRequisitions(facility, program, null, null,
        processingPeriod, null, null, null);
  }

  /**
   * Finds requisitions matching all of the provided parameters.
   */
  public List<Requisition> searchRequisitions(Set<RequisitionStatus> requisitionStatuses) {
    return requisitionRepository.searchRequisitions(null, null, null, null, null,
        null, requisitionStatuses, null);
  }

  /**
   * Finds requisitions matching supervisory node and program.
   */
  public List<Requisition> searchRequisitions(UUID program, UUID supervisoryNode) {
    return requisitionRepository.searchRequisitions(null, program, null, null, null,
        supervisoryNode, null, null);
  }

  /**
   * Get requisitions to approve for the specified user.
   */
  public Set<Requisition> getRequisitionsForApproval(UUID userId, UUID program) {
    Set<Requisition> requisitionsForApproval = new LinkedHashSet<>();
    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);
    List<DetailedRoleAssignmentDto> roleAssignments = userRoleAssignmentsReferenceDataService
        .getRoleAssignments(userId)
        .stream()
        .filter(r -> r.getRole().getRights().contains(right))
        .collect(Collectors.toList());

    if (roleAssignments != null) {
      for (DetailedRoleAssignmentDto roleAssignment : roleAssignments) {
        if (roleAssignment.getSupervisoryNodeId() != null
            && roleAssignment.getProgramId() != null
                && (program == null || program.equals(roleAssignment.getProgramId()))) {
          requisitionsForApproval.addAll(getApprovableRequisitions(
              roleAssignment.getProgramId(), roleAssignment.getSupervisoryNodeId()));
        }
      }
    }
    return requisitionsForApproval;
  }

  /**
   * Get approvable requisitions for specified program and supervisoryNode.
   */
  public List<Requisition> getApprovableRequisitions(UUID programId, UUID supervisoryNodeId) {
    List<Requisition> requisitions = searchRequisitions(programId, supervisoryNodeId);

    List<Requisition> filteredRequisitions = new ArrayList<>();
    if (requisitions != null) {
      for (Requisition requisition : requisitions) {
        if (requisition.isApprovable()) {
          filteredRequisitions.add(requisition);
        }
      }
    }
    return filteredRequisitions;
  }

  /**
   * Checks if given user has permission to approve requisition, if not exception is thrown.
   *
   * @param programId         UUID of program that requisition is assigned to
   * @param supervisoryNodeId UUID of supervisory node where requisition have to be approved
   * @param userId            UUID of user that wants to approve requisition
   */
  public void checkIfCanApproveRequisition(UUID programId, UUID supervisoryNodeId, UUID userId) {
    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);

    if (!userRoleAssignmentsReferenceDataService.hasSupervisionRight(right, userId,
        programId, supervisoryNodeId)) {
      throw new PermissionMessageException(
          new Message(MessageKeys.ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION));
    }
  }

  /**
   * Releases the list of given requisitions as order.
   *
   * @param convertToOrderDtos list of Requisitions with their supplyingDepots to be released as
   *                           order
   * @return list of released requisitions
   */
  public List<Requisition> releaseRequisitionsAsOrder(
      List<ConvertToOrderDto> convertToOrderDtos, UserDto user) {
    RightDto right = authenticationHelper.getRight(RightName.ORDERS_EDIT);
    List<Requisition> releasedRequisitions = new ArrayList<>();
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), right.getId()).stream().map(FacilityDto::getId)
        .collect(Collectors.toSet());

    for (ConvertToOrderDto convertToOrderDto : convertToOrderDtos) {
      UUID requisitionId = convertToOrderDto.getRequisitionId();
      Requisition loadedRequisition = requisitionRepository.findOne(requisitionId);

      if (RequisitionStatus.APPROVED == loadedRequisition.getStatus()) {
        loadedRequisition.release(authenticationHelper.getCurrentUser().getId());
      } else {
        throw new ValidationMessageException(new Message(ERROR_REQUISITION_MUST_BE_APPROVED,
            loadedRequisition.getId()));
      }

      UUID facilityId = convertToOrderDto.getSupplyingDepotId();
      Set<UUID> validFacilities = getAvailableSupplyingDepots(requisitionId)
          .stream().filter(f -> userFacilities.contains(f.getId())).map(FacilityDto::getId)
          .collect(Collectors.toSet());

      if (validFacilities.contains(facilityId)) {
        loadedRequisition.setSupplyingFacilityId(facilityId);
      } else {
        throw new ValidationMessageException(new Message(ERROR_MUST_HAVE_SUPPLYING_FACILITY,
            loadedRequisition.getId()));
      }

      releasedRequisitions.add(loadedRequisition);
    }

    return releasedRequisitions;
  }

  /**
   * Retrieves available supplying depots for given requisition.
   *
   * @param requisitionId id of requisition to find facilities for
   * @return list of facilities
   */
  public List<FacilityDto> getAvailableSupplyingDepots(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    Collection<FacilityDto> facilityDtos = facilityReferenceDataService
        .searchSupplyingDepots(requisition.getProgramId(), requisition.getSupervisoryNodeId());
    return new ArrayList<>(facilityDtos);
  }

  /**
   * Retrieves full supply requisition line items for requisition with given id.
   *
   * @param requisitionId id of requisition to find items for.
   * @return list of full supply requisition line items.
   */
  public List<RequisitionLineItem> getFullSupplyItems(UUID requisitionId) {
    return getSupplyItemsBase(requisitionId, true);
  }

  /**
   * Retrieves non-full supply requisition line items for requisition with given id.
   *
   * @param requisitionId id of requisition to find items for.
   * @return list of non-full supply requisition line items.
   */
  public List<RequisitionLineItem> getNonFullSupplyItems(UUID requisitionId) {
    return getSupplyItemsBase(requisitionId, false);
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy    Field used to filter: "programName", "facilityCode", "facilityName" or
   *                    "all".
   * @param sortBy      Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending  Descending direction for sort.
   * @param pageable    Pageable object that allows to optionally add "page" (page number)
   *                     and "size" (page size) query parameters.
   * @param userManagedFacilities List of UUIDs of facilities that are managed by logged in user.
   * @return List of requisitions.
   */
  public Page<RequisitionWithSupplyingDepotsDto>
      searchApprovedRequisitionsWithSortAndFilterAndPaging(String filterValue,
                                                           String filterBy,
                                                           String sortBy,
                                                           Boolean descending,
                                                           Pageable pageable,
                                                           Collection<UUID> userManagedFacilities) {
    List<UUID> desiredUuids = findDesiredUuids(filterValue, filterBy);
    List<Requisition> requisitionsList =
        requisitionRepository.searchApprovedRequisitions(filterBy, desiredUuids);
    List<RequisitionDto> requisitionDtosList =
        convertHelper.convertRequisitionListToRequisitionDtoList(requisitionsList);

    requisitionDtosList.sort(new RequisitionDtoComparator(sortBy));
    if (descending) {
      Collections.reverse(requisitionDtosList);
    }

    List<RequisitionWithSupplyingDepotsDto> responseList = new ArrayList<>();
    for (RequisitionDto requisition : requisitionDtosList) {
      List<FacilityDto> facilities = getAvailableSupplyingDepots(requisition.getId())
          .stream()
          .filter(f -> userManagedFacilities.contains(f.getId()))
          .collect(Collectors.toList());

      if (!facilities.isEmpty()) {
        responseList.add(new RequisitionWithSupplyingDepotsDto(requisition, facilities));
      }
    }

    return Pagination.getPage(responseList, pageable);
  }

  /**
   * Converting Requisition list to Orders.
   */
  @Transactional
  public void convertToOrder(List<ConvertToOrderDto> list, UserDto user) {
    List<Requisition> releasedRequisitions = releaseRequisitionsAsOrder(list, user);

    List<OrderDto> orders = new ArrayList<>();
    for (Requisition requisition : releasedRequisitions) {
      OrderDto order = orderDtoBuilder.build(requisition, user);
      orders.add(order);

      requisitionRepository.save(requisition);
      requisitionStatusProcessor.statusChange(requisition);
    }

    orderFulfillmentService.create(orders);
  }

  private List<RequisitionLineItem> getSupplyItemsBase(UUID requisitionId, boolean fullSupply) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();

    for (RequisitionLineItem lineItem : requisition.getRequisitionLineItems()) {
      OrderableDto orderable = orderableReferenceDataService
          .findOne(lineItem.getOrderableId());

      Optional<ProgramOrderableDto> product = orderable.getPrograms().stream()
          .filter(e -> requisition.getProgramId().equals(e.getProgramId())).findFirst();

      product.ifPresent(p -> {
        if (p.getFullSupply() == fullSupply) {
          requisitionLineItems.add(lineItem);
        }
      });
    }

    return requisitionLineItems;
  }

  private List<UUID> findDesiredUuids(String filterValue, String filterBy) {
    List<UUID> uuidsToReturn = new ArrayList<>();
    boolean filterAll = "all".equals(filterBy);
    boolean filterByCode = "facilityCode".equals(filterBy);
    boolean filterByName = "facilityName".equals(filterBy);

    if (filterAll || "programName".equalsIgnoreCase(filterBy)) {
      Collection<ProgramDto> foundPrograms = programReferenceDataService.search(filterValue);
      foundPrograms.forEach(programDto -> uuidsToReturn.add(programDto.getId()));
    }

    Collection<FacilityDto> foundFacilities = new ArrayList<>();
    if (filterAll && filterValue.isEmpty()) {
      foundFacilities.addAll(facilityReferenceDataService.findAll());
    } else {
      if (filterAll || filterByCode) {
        foundFacilities.addAll(facilityReferenceDataService.search(filterValue, null, null, false));
      }

      if (filterAll || filterByName) {
        foundFacilities.addAll(facilityReferenceDataService.search(null, filterValue, null, false));
      }
    }

    foundFacilities.forEach(facilityDto -> uuidsToReturn.add(facilityDto.getId()));
    return uuidsToReturn;
  }

  private List<Requisition> getRecentRequisitions(Requisition requisition, int amount) {
    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(requisition.getProcessingPeriodId(), amount);

    List<Requisition> recentRequisitions = new ArrayList<>();
    for (ProcessingPeriodDto period : previousPeriods) {
      List<Requisition> requisitionsByPeriod = getRequisitionsByPeriod(requisition, period);
      if (!requisitionsByPeriod.isEmpty()) {
        Requisition requisitionByPeriod = requisitionsByPeriod.get(0);
        recentRequisitions.add(requisitionByPeriod);
      }
    }
    return recentRequisitions;
  }

  private List<Requisition> getRequisitionsByPeriod(Requisition requisition,
                                                    ProcessingPeriodDto period) {
    return searchRequisitions(requisition.getFacilityId(),
        requisition.getProgramId(),
        period.getId());
  }

}
