package org.openlmis.requisition.service;

import org.apache.commons.lang3.ObjectUtils;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ScheduleReferenceDataService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class PeriodService {

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private ScheduleReferenceDataService scheduleReferenceDataService;

  public Collection<ProcessingPeriodDto> search(UUID scheduleId, LocalDate startDate) {
    return periodReferenceDataService.search(scheduleId, startDate);
  }

  public Collection<ProcessingPeriodDto> searchByProgramAndFacility(UUID programId,
                                                                    UUID facilityId) {
    return periodReferenceDataService.searchByProgramAndFacility(programId, facilityId);
  }

  public ProcessingPeriodDto getPeriod(UUID periodId) {
    return periodReferenceDataService.findOne(periodId);
  }

  /**
   * Find and return a list of current processing periods.
   *
   * @param programId  UUID of Program.
   * @param facilityId UUID of Facility.
   * @return a list of current processing periods
   */
  public List<ProcessingPeriodDto> getCurrentPeriods(UUID programId, UUID facilityId) {
    Collection<ProcessingPeriodDto> periods = searchByProgramAndFacility(programId, facilityId);

    return periods
        .stream()
        .filter(period -> {
          // check if period is in current date. For example if currently we have
          // 10th November 2016 then only periods that have start date before (or equal to) current
          // date and end date after (or equal to) current date should be processed.
          LocalDate currentDate = LocalDate.now();

          return !currentDate.isBefore(period.getStartDate())
              && !currentDate.isAfter(period.getEndDate());
        })
        .filter(period -> {
          // check if regular requisition with the period are submitted. If the given period does
          // not have a regular requisition or the regular requisition has pre submitted status
          // then that period should be omitted.
          List<Requisition> requisitions =
              requisitionRepository.searchByProcessingPeriodAndType(period.getId(), false);

          return !(null == requisitions || requisitions.isEmpty())
              && requisitions.stream().allMatch(Requisition::isPostSubmitted);
        })
        .collect(Collectors.toList());
  }

  /**
   * Gets a period list for the given program and facility.
   *
   * @param program   UUID of Program.
   * @param facility  UUID of Facility.
   * @param emergency decide if periods should be find for standard or emergency requisitions.
   * @return a list of periods.
   */
  public Collection<ProcessingPeriodDto> getPeriods(UUID program, UUID facility,
                                                    boolean emergency) {
    Collection<ProcessingPeriodDto> periods;

    if (emergency) {
      periods = getCurrentPeriods(program, facility);
    } else {
      periods = searchByProgramAndFacility(program, facility);

      for (Iterator<ProcessingPeriodDto> iterator = periods.iterator(); iterator.hasNext(); ) {
        ProcessingPeriodDto periodDto = iterator.next();
        List<Requisition> requisitions =
            requisitionRepository.searchByProcessingPeriodAndType(periodDto.getId(), false);

        if (requisitions != null && !requisitions.isEmpty()
            && requisitions.get(0).getStatus() != RequisitionStatus.INITIATED
            && requisitions.get(0).getStatus() != RequisitionStatus.SUBMITTED) {
          iterator.remove();
        }
      }
    }

    return periods;
  }

  /**
   * Find previous period for the given period.
   *
   * @param periodId UUID of period
   * @return previous period or {@code null} if not found.
   */
  public ProcessingPeriodDto findPreviousPeriod(UUID periodId) {
    List<ProcessingPeriodDto> previousPeriods = findPreviousPeriods(periodId, 1);
    return previousPeriods.isEmpty() ? null : previousPeriods.get(0);
  }

  /**
   * Find recent periods for the given period.
   *
   * @param periodId UUID of period
   * @param amount of previous periods
   * @return previous period or {@code null} if not found.
   */
  public List<ProcessingPeriodDto> findPreviousPeriods(UUID periodId, int amount) {
    // retrieve data from reference-data
    ProcessingPeriodDto period = getPeriod(periodId);

    if (null == period) {
      return Collections.emptyList();
    }

    Collection<ProcessingPeriodDto> collection = search(
        period.getProcessingSchedule().getId(), period.getStartDate()
    );

    if (null == collection || collection.isEmpty()) {
      return Collections.emptyList();
    }

    // create a list...
    List<ProcessingPeriodDto> list = new ArrayList<>(collection);
    // ...remove the latest period from the list...
    list.removeIf(p -> p.getId().equals(periodId));
    // .. and sort elements by startDate property DESC.
    list.sort((one, two) -> ObjectUtils.compare(two.getStartDate(), one.getStartDate()));

    if (amount > list.size()) {
      return list;
    }
    return list.subList(0, amount);
  }

  /**
   * Find a period based on passed arguments.
   *
   * @param programId         UUID of program
   * @param facilityId        UUID of facility
   * @param suggestedPeriodId UUID of suggested period
   * @param emergency         true if requisition has emergency flag; otherwise false.
   * @return an instance of {@link ProcessingPeriodDto}
   * @throws ValidationMessageException         if period cannot be found, period has different id
   *                                            than suggested period or processing schedule of
   *                                            found period has different ID than retrieved
   *                                            schedule.
   */
  public ProcessingPeriodDto findPeriod(UUID programId, UUID facilityId, UUID suggestedPeriodId,
                                        Boolean emergency) {
    ProcessingPeriodDto period;

    if (emergency) {
      List<ProcessingPeriodDto> periods = getCurrentPeriods(programId, facilityId);

      if (periods.isEmpty()) {
        throw new ValidationMessageException(
            new Message("requisition.error.initiate.cannot-find-current-period"));
      }

      period = periods.get(0);
    } else {
      period = findTheOldestPeriod(programId, facilityId);
    }

    if (period == null
        || (null != suggestedPeriodId && !suggestedPeriodId.equals(period.getId()))) {
      throw new ValidationMessageException(new Message(
          "requisition.error.initiate.period-should-be-oldest-and-not-associated"));
    }

    Collection<ProcessingScheduleDto> schedules =
        scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId);

    if (schedules == null || schedules.isEmpty()) {
      throw new ContentNotFoundMessageException(new Message(
          "requisition.error.initiate"
              + ".requisition-group-program-schedule-with-program-anf-facility-not-found"));
    }

    ProcessingScheduleDto scheduleDto = schedules.iterator().next();

    if (!scheduleDto.getId().equals(period.getProcessingSchedule().getId())) {
      throw new ValidationMessageException(new Message(
          "requisition.error.initiate.period-must-belong-to-the-same-schedule"));
    }

    return period;
  }

  /**
   * Return the oldest period which is not associated with any requisition.
   *
   * @param programId  Program for Requisition
   * @param facilityId Facility for Requisition
   * @return ProcessingPeriodDto.
   */
  private ProcessingPeriodDto findTheOldestPeriod(UUID programId, UUID facilityId) {

    Requisition lastRequisition = requisitionRepository.getLastRegularRequisition(
        facilityId, programId
    );

    if (null != lastRequisition && lastRequisition.isPreAuthorize()) {
      throw new ValidationMessageException(new Message(
          "requisition.error.initiate.finish-previous-requisition"));
    }

    ProcessingPeriodDto result = null;
    Collection<ProcessingPeriodDto> periods = searchByProgramAndFacility(programId, facilityId);

    List<Requisition> requisitions;

    if (periods != null) {
      for (ProcessingPeriodDto dto : periods) {
        requisitions = requisitionRepository.searchByProcessingPeriodAndType(dto.getId(), false);
        if (requisitions == null || requisitions.isEmpty()) {
          result = dto;
          break;
        }
      }
    }

    return result;
  }

}
