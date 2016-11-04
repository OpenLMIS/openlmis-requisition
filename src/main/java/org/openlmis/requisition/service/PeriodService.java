package org.openlmis.requisition.service;

import org.apache.commons.lang3.ObjectUtils;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
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
    // retrieve data from reference-data
    ProcessingPeriodDto period = getPeriod(periodId);

    if (null == period) {
      return null;
    }

    Collection<ProcessingPeriodDto> collection = search(
        period.getProcessingSchedule().getId(), period.getStartDate()
    );

    if (null == collection || collection.isEmpty()) {
      return null;
    }

    // create a list...
    List<ProcessingPeriodDto> list = new ArrayList<>(collection);
    // ...remove the latest period from the list because it is not previous...
    list.removeIf(p -> p.getId().equals(periodId));
    // .. and sort elements by startDate property DESC.
    list.sort((one, two) -> ObjectUtils.compare(two.getStartDate(), one.getStartDate()));

    // The latest previous date should be first.
    return list.isEmpty() ? null : list.get(0);
  }

}
