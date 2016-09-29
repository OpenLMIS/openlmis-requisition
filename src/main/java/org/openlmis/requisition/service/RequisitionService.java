package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionAlreadyExistsException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionInitializationException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.exception.RequisitionTemplateNotFoundException;
import org.openlmis.requisition.exception.SkipNotAllowedException;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
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
  private static final String REQUISITION_BAD_STATUS_MESSAGE = "requisition has bad status";

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private RequisitionLineItemService requisitionLineItemService;

  @Autowired
  private RequisitionLineItemRepository requisitionLineItemRepository;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  /**
   * Initiated given requisition if possible.
   *
   * @param programId         UUID of Program.
   * @param facilityId        UUID of Facility.
   * @param emergency         Emergency status.
   * @param suggestedPeriodId Period for requisition.
   * @return Initiated requisition.
   * @throws RequisitionException Exception thrown when it is not possible to initialize a
   *                              requisition.
   */
  public Requisition initiate(UUID programId, UUID facilityId, UUID suggestedPeriodId,
                              Boolean emergency) throws RequisitionException {

    RequisitionTemplate requisitionTemplate = findRequisitionTemplate(programId);
    Requisition requisition;

    if (facilityId == null || programId == null || emergency == null) {
      throw new RequisitionInitializationException(
          "Requisition cannot be initiated with null object"
      );
    } else if (facilityReferenceDataService.findOne(facilityId) != null
        && programReferenceDataService.findOne(programId) != null) {
      requisition = new Requisition();
      requisition.setStatus(RequisitionStatus.INITIATED);
      requisition.setEmergency(emergency);
      requisition.setFacility(facilityId);
      requisition.setProgram(programId);

      //ProcessingPeriodDto period = findPeriod(facilityId, programId, emergency);
      //if (suggestedPeriodId != null) {
      //  if (suggestedPeriodId != period.getId()) {
      //    period = suggestedPeriodId;
      //  }
      //}
      //TODO requisition.setProcessingPeriod();
      //TODO setlineitem(template)
      requisitionLineItemService.initiateRequisitionLineItemFields(requisition,
          requisitionTemplate);

      requisition.getRequisitionLineItems().forEach(
          requisitionLineItem -> requisitionLineItemRepository.save(requisitionLineItem));
      requisitionRepository.save(requisition);
    } else {
      throw new RequisitionAlreadyExistsException("Cannot initiate requisition."
          + " Requisition with such parameters already exists");
    }

    return requisition;
  }

  private RequisitionTemplate findRequisitionTemplate(UUID programId) throws RequisitionException {
    if (null == programId) {
      throw new IllegalArgumentException("program ID cannot be null");
    }

    List<RequisitionTemplate> requisitionTemplates =
        requisitionTemplateService.searchRequisitionTemplates(programId);

    if (null == requisitionTemplates || requisitionTemplates.isEmpty()) {
      throw new RequisitionTemplateNotFoundException("RequisitionTemplate not found");
    }

    RequisitionTemplate template = requisitionTemplates.get(0);

    if (template.getColumnsMap().isEmpty()) {
      throw new RequisitionTemplateNotFoundException("RequisitionTemplate is not defined");
    } else {
      return template;
    }
  }

  /**
   * Delete given Requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be deleted.
   * @throws RequisitionException Exception thrown when it is not possible to delete a requisition.
   */
  public void delete(UUID requisitionId) throws RequisitionException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new RequisitionNotFoundException(requisitionId);
    } else if (requisition.getStatus() != RequisitionStatus.INITIATED) {
      throw new InvalidRequisitionStatusException("Delete failed - "
          + REQUISITION_BAD_STATUS_MESSAGE);
    } else {
      requisitionRepository.delete(requisition);
      LOGGER.debug("Requisition deleted");
    }
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
      throw new RequisitionNotFoundException(requisitionId);
    } else {
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgram());
      if (requisition.getStatus() != RequisitionStatus.INITIATED) {
        throw new InvalidRequisitionStatusException("Skip failed - "
            + REQUISITION_BAD_STATUS_MESSAGE);
      } else if (!program.getPeriodsSkippable()) {
        throw new SkipNotAllowedException("Skip failed - "
            + "requisition program does not allow skipping");
      } else {
        LOGGER.debug("Requisition skipped");
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
      throw new RequisitionNotFoundException(requisitionId);
    } else if (requisition.getStatus() == RequisitionStatus.AUTHORIZED
        || requisition.getStatus() == RequisitionStatus.SUBMITTED) {
      LOGGER.debug("Requisition rejected: " + requisitionId);
      requisition.setStatus(RequisitionStatus.INITIATED);
      return requisitionRepository.save(requisition);
    } else {
      throw new InvalidRequisitionStatusException("Cannot reject requisition: " + requisitionId
          + " .Requisition must be waiting for approval to be rejected");
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
    for (ProgramDto program : supervisedPrograms) {
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
        throw new InvalidRequisitionStatusException("Can not release requisition:"
            + loadedRequisition.getId()
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
   * @param filterBy    Field used to filter: "programName", "facilityCode", "facilityName" or
   *                    "all".
   * @param sortBy      Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending  Descending direction for sort.
   * @param pageNumber  Page number to return.
   * @param pageSize    Quantity for one page.
   * @return List of requisitions.
   */
  public List<Requisition> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      String filterValue, String filterBy, String sortBy, Boolean descending,
      Integer pageNumber, Integer pageSize) {

    return requisitionRepository.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        filterValue, filterBy, sortBy, descending, pageNumber, pageSize);
  }
}
