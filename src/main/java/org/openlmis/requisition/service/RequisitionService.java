package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
public class RequisitionService {
  private static final String REQUISITION_NULL_MESSAGE = "requisition cannot be null";
  private static final String REQUISITION_DOES_NOT_EXISTS_MESSAGE = "Requisition does not exist: ";
  private static final String REQUISITION_BAD_STATUS_MESSAGE = "requisition has bad status";

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionLineItemService requisitionLineItemService;

  @Autowired
  private RequisitionLineItemRepository requisitionLineItemRepository;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  /**
   * Initiated given requisition if possible.
   *
   * @param requisitionDto Requisition object to initiate.
   * @return Initiated requisition.
   * @throws RequisitionException Exception thrown when
   *      it is not possible to initialize a requisition.
   */
  public Requisition initiateRequisition(Requisition requisitionDto)
                                          throws RequisitionException {

    if (requisitionDto == null) {
      throw new RequisitionException("Requisition cannot be initiated with null object");
    } else if (requisitionRepository.findOne(requisitionDto.getId()) == null) {

      requisitionDto.setStatus(RequisitionStatus.INITIATED);
      requisitionLineItemService.initiateRequisitionLineItemFields(requisitionDto);

      requisitionDto.getRequisitionLineItems().forEach(
          requisitionLineItem -> requisitionLineItemRepository.save(requisitionLineItem));
      requisitionRepository.save(requisitionDto);

    } else {
      throw new RequisitionException("Cannot initiate requisition."
          + " Requisition with such parameters already exists");
    }

    return requisitionDto;
  }

  /**
   * Delete given Requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be deleted.
   * @return True if deletion successful, false otherwise.
   * @throws RequisitionException Exception thrown when it is not possible to delete a requisition.
   */
  public boolean tryDelete(UUID requisitionId) throws RequisitionException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new RequisitionException(REQUISITION_DOES_NOT_EXISTS_MESSAGE + requisitionId);
    } else if (requisition.getStatus() != RequisitionStatus.INITIATED) {
      LOGGER.debug("Delete failed - " + REQUISITION_BAD_STATUS_MESSAGE);
    } else {
      requisitionRepository.delete(requisition);
      LOGGER.debug("Requisition deleted");
      return true;
    }
    return false;
  }

  /**
   * Skip given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be skipped.
   * @return Skipped Requisition.
   * @throws RequisitionException Exception thrown when it is not possible to skip a requisition.
   */
  public Requisition skip(UUID requisitionId) throws RequisitionException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new RequisitionException("Skip failed - "
          + REQUISITION_NULL_MESSAGE);
    } else {
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgram());
      if (requisition.getStatus() != RequisitionStatus.INITIATED) {
        throw new RequisitionException("Skip failed - "
            + REQUISITION_BAD_STATUS_MESSAGE);
      } else if (!program.getPeriodsSkippable()) {
        throw new RequisitionException("Skip failed - "
            + "requisition program does not allow skipping");
      } else {
        LOGGER.info("Requisition skipped");
        requisition.setStatus(RequisitionStatus.SKIPPED);
        return requisitionRepository.save(requisition);
      }
    }
  }

  /**
   * Reject given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be rejected.
   * @throws RequisitionException Exception thrown when it is not possible to reject a requisition.
   */
  public Requisition reject(UUID requisitionId) throws RequisitionException {

    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new RequisitionException(REQUISITION_DOES_NOT_EXISTS_MESSAGE + requisitionId);
    } else if (requisition.getStatus() == RequisitionStatus.AUTHORIZED
            || requisition.getStatus() == RequisitionStatus.SUBMITTED) {
      LOGGER.debug("Requisition rejected: " + requisitionId);
      requisition.setStatus(RequisitionStatus.INITIATED);
      return requisitionRepository.save(requisition);
    } else {
      throw new RequisitionException("Cannot reject requisition: " + requisitionId
              + " .Requisition must be waiting for approval or authorization to be rejected");
    }
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  public List<Requisition> searchRequisitions(UUID facility, UUID program,
                                              LocalDateTime createdDateFrom,
                                              LocalDateTime createdDateTo,
                                              UUID processingPeriod,
                                              UUID supervisoryNode,
                                              RequisitionStatus requisitionStatus) {
    return requisitionRepository.searchRequisitions(
            facility, program, createdDateFrom,
            createdDateTo, processingPeriod, supervisoryNode, requisitionStatus);
  }

  /**
   * Get requisitions to approve for specified user.
   */
  public List<Requisition> getRequisitionsForApproval(UUID userId) {
    UserDto user = userReferenceDataService.findOne(userId);
    List<Requisition> requisitionsForApproval = new ArrayList<>();
    Set<ProgramDto> supervisedPrograms = user.getSupervisedPrograms();
    for (ProgramDto program: supervisedPrograms) {
      requisitionsForApproval.addAll(getAuthorizedRequisitions(program));
    }
    return requisitionsForApproval;
  }

  /**
   * Get authorized requisitions for specified program.
   */
  public List<Requisition> getAuthorizedRequisitions(ProgramDto program) {
    List<Requisition> requisitions = new ArrayList<>();
    List<Requisition> reqList = searchRequisitions(null, program.getId(),
        null, null, null, null, null);
    if (reqList != null) {
      for (Requisition req : reqList) {
        if (req.getStatus() == RequisitionStatus.AUTHORIZED) {
          requisitions.add(req);
        }
      }
    }
    return requisitions;
  }

  /**
   * Releases the list of given requisitions as order.
   *
   * @param requisitionList list of requisitions to be released as order
   * @return list of released requisitions
   */
  public List<Requisition> releaseRequisitionsAsOrder(List<Requisition> requisitionList)
          throws RequisitionException {
    List<Requisition> releasedRequisitions = new ArrayList<>();
    for (Requisition requisition : requisitionList) {
      Requisition loadedRequisition = requisitionRepository.findOne(requisition.getId());
      if (RequisitionStatus.APPROVED == loadedRequisition.getStatus()) {
        loadedRequisition.setStatus(RequisitionStatus.RELEASED);
        releasedRequisitions.add(requisitionRepository.save(loadedRequisition));
      } else {
        throw new RequisitionException("Can not release requisition:" + loadedRequisition.getId()
                + " as order. Requisition must be approved.");
      }
    }
    return releasedRequisitions;
  }

  private Requisition save(Requisition requisition) throws RequisitionException {
    if (requisition != null) {
      if (requisition.getRequisitionLineItems() != null) {
        for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
          requisitionLineItemService.save(requisition, requisitionLineItem);
        }
      }
      return requisitionRepository.save(requisition);
    } else {
      return null;
    }
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy Field used to filter: "programName", "facilityCode", "facilityName" or "all".
   * @param sortBy Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending Descending direction for sort.
   * @param pageNumber Page number to return.
   * @param pageSize Quantity for one page.
   *
   * @return List of requisitions.
   */
  public List<Requisition> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      String filterValue, String filterBy, String sortBy, Boolean descending,
      Integer pageNumber, Integer pageSize) {

    return requisitionRepository.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        filterValue, filterBy, sortBy, descending, pageNumber, pageSize);
  }
}
