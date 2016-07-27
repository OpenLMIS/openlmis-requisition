package org.openlmis.requisition.service;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class RequisitionService {
  private final String requisitionNullMessage = "requisition cannot be null";
  private final String requisitionNotExistsMessage = "Requisition does not exists: ";
  private final String requisitionBadStatusMessage = "requisition has bad status";

  private Logger logger = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  PeriodRepository periodRepository;

  @Autowired
  FacilityRepository facilityRepository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  RequisitionLineService requisitionLineService;

  @PersistenceContext
  EntityManager entityManager;

  public Requisition initiateRequisition(UUID facilityId,
                                         UUID programId, UUID periodId, boolean emergency) {

    Program program = programRepository.findOne(programId);
    Facility facility = facilityRepository.findOne(facilityId);
    Period period = periodRepository.findOne(periodId);

    if (facility == null) {
      throw new RequisitionException("Facility with facility Id: "
          + facilityId + ", does not exists");
    }
    if (program == null) {
      throw new RequisitionException("Program with program Id: " + programId + ", does not exists");
    }
    if (period == null) {
      throw new RequisitionException("Period with period Id: " + periodId + ", does not exists");
    }

    Requisition requisition = requisitionRepository.
        findByProcessingPeriodAndFacilityAndProgram(period, facility, program);

    if (emergency || requisition == null) {
      requisition = new Requisition();

      requisition.setStatus(RequisitionStatus.INITIATED);
      requisition.setProgram(program);
      requisition.setFacility(facility);
      requisition.setProcessingPeriod(period);

      requisitionLineService.initiateRequisitionLineFields(requisition);
      requisitionRepository.save(requisition);
    } else {
      throw new RequisitionException("Cannot initiate requisition."
          + " Non emergency requisition with such parameters already exists");
    }

    return requisition;
  }

  public Requisition submitRequisition(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new RequisitionException(requisitionNotExistsMessage + requisitionId);
    } else {
      logger.debug("Submitting a requisition with id " + requisitionId);
      requisition.setStatus(RequisitionStatus.SUBMITTED);
      requisitionRepository.save(requisition);
      logger.debug("Requisition with id " + requisitionId + " submitted");
      return requisition;
    }
  }

  public boolean tryDelete(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new RequisitionException(requisitionNotExistsMessage + requisitionId);
    } else if (requisition.getStatus() != RequisitionStatus.INITIATED) {
      logger.debug("Delete failed - " + requisitionBadStatusMessage);
    } else {
      requisitionRepository.delete(requisition);
      logger.debug("Requisition deleted");
      return true;
    }

    return false;
  }

  public boolean skip(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      logger.debug("Skip failed - "
          + requisitionNullMessage);
    } else if (requisition.getStatus() != RequisitionStatus.INITIATED) {
      logger.debug("Skip failed - "
          + requisitionBadStatusMessage);
    } else if (!requisition.getProgram().getPeriodsSkippable()) {
      logger.debug("Skip failed - "
              + "requisition program does not allow skipping");
    } else {
      logger.debug("Requisition skipped");
      requisition.setStatus(RequisitionStatus.SKIPPED);
      requisitionRepository.save(requisition);
      return true;
    }
    return false;
  }

  public void reject(UUID requisitionId) {

    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new RequisitionException(requisitionNotExistsMessage + requisitionId);
    } else if (requisition.getStatus() != RequisitionStatus.AUTHORIZED) {
      throw new RequisitionException("Cannot reject requisition: " + requisitionId 
          + " .Requisition must be waiting for approval to be rejected");
    } else {
      logger.debug("Requisition rejected: " + requisitionId);
      requisition.setStatus(RequisitionStatus.INITIATED);
      requisitionRepository.save(requisition);
    }
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  public List<Requisition> searchRequisitions(Facility facility, Program program,
                                              LocalDateTime createdDateFrom,
                                              LocalDateTime createdDateTo) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    Root<Requisition> root = query.from(Requisition.class);

    Predicate predicate = builder.conjunction();
    if (facility != null) {
      predicate = builder.and(predicate, builder.equal(root.get("facility"), facility));
    }
    if (program != null) {
      predicate = builder.and(predicate, builder.equal(root.get("program"), program));
    }
    if (createdDateFrom != null) {
      predicate = builder.and(predicate,
          builder.greaterThanOrEqualTo(root.get("createdDate"), createdDateFrom));
    }
    if (createdDateTo != null) {
      predicate = builder.and(predicate,
          builder.lessThanOrEqualTo(root.get("createdDate"), createdDateTo));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

  public Requisition authorize(UUID requisitionId, Requisition requisitionDto,
                               boolean validationErrors) {

    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new RequisitionException(requisitionNotExistsMessage + requisitionId);
    } else if (requisition.getStatus() != RequisitionStatus.SUBMITTED) {
      throw new RequisitionException("Cannot authorize requisition: " + requisitionId
        + " . Requisition must have submitted status to be authorized");
    } else if (requisitionDto == null || validationErrors) {
      throw new RequisitionException("Requisition object is not valid.");
    } else {
      requisitionDto.setStatus(RequisitionStatus.AUTHORIZED);
      return requisitionRepository.save(requisitionDto);
    }
  }

  /**
   * Releasing the Requisitions.
   */
  public void releaseRequisitionsAsOrder(List<Requisition> requisitionList) {
    for (Requisition requisition : requisitionList) {
      Requisition loadedRequisition = requisitionRepository.findOne(requisition.getId());
      loadedRequisition.setStatus(RequisitionStatus.RELEASED);
      requisitionRepository.save(loadedRequisition);
    }
  }

  private Requisition save(Requisition requisition) {
    if (requisition != null) {
      if (requisition.getRequisitionLines() != null) {
        for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
          requisitionLineService.save(requisition,requisitionLine);
        }
      }
      return requisitionRepository.save(requisition);
    } else {
      return null;
    }
  }

}
