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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FINISH_PROVIOUS_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_SUGGESTED_PERIOD;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_MUST_BELONG_TO_THE_SAME_SCHEDULE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_SHOULD_BE_OLDEST_AND_NOT_ASSOCIATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_GROUP_PROGRAM_SCHEDULE_WITH_PROGRAM_AND_FACILITY_NOT_FOUND;

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
import org.openlmis.requisition.utils.Message;
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
  public Collection<ProcessingPeriodDto> getPeriods(
      UUID program, UUID facility, boolean emergency) {
    Collection<ProcessingPeriodDto> periods;

    if (emergency) {
      periods = getCurrentPeriods(program, facility);
    } else {
      periods = searchByProgramAndFacility(program, facility);

      for (Iterator<ProcessingPeriodDto> iterator = periods.iterator(); iterator.hasNext(); ) {
        ProcessingPeriodDto periodDto = iterator.next();
        List<Requisition> requisitions =
            requisitionRepository.searchRequisitions(
                periodDto.getId(), facility, program, false);

        if (requisitions != null && !requisitions.isEmpty()
            && !requisitions.get(0).getStatus().isPreAuthorize()) {
          iterator.remove();
        }
      }
    }

    return periods;
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
        throw new ValidationMessageException(new Message(ERROR_INCORRECT_SUGGESTED_PERIOD));
      }

      period = periods.get(0);
    } else {
      period = findCurrentPeriodForInitiate(programId, facilityId);
    }

    if (period == null
        || (null != suggestedPeriodId && !suggestedPeriodId.equals(period.getId()))) {
      throw new ValidationMessageException(new Message(
          ERROR_PERIOD_SHOULD_BE_OLDEST_AND_NOT_ASSOCIATED));
    }

    Collection<ProcessingScheduleDto> schedules =
        scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId);

    if (schedules == null || schedules.isEmpty()) {
      throw new ContentNotFoundMessageException(new Message(
          ERROR_REQUISITION_GROUP_PROGRAM_SCHEDULE_WITH_PROGRAM_AND_FACILITY_NOT_FOUND));
    }

    ProcessingScheduleDto scheduleDto = schedules.iterator().next();

    if (!scheduleDto.getId().equals(period.getProcessingSchedule().getId())) {
      throw new ValidationMessageException(new Message(
          ERROR_PERIOD_MUST_BELONG_TO_THE_SAME_SCHEDULE));
    }

    return period;
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
   * Return the oldest period which is not associated with any requisition.
   *
   * @param programId  Program for Requisition
   * @param facilityId Facility for Requisition
   * @return ProcessingPeriodDto.
   */
  private ProcessingPeriodDto findCurrentPeriodForInitiate(UUID programId, UUID facilityId) {
    ProcessingPeriodDto result = null;
    Collection<ProcessingPeriodDto> periods = searchByProgramAndFacility(programId, facilityId);

    if (periods != null) {
      RequisitionStatus previousStatus = null;

      for (ProcessingPeriodDto dto : periods) {
        // There is always maximum one regular requisition for given period, facility and program
        List<Requisition> requisitions = requisitionRepository.searchRequisitions(
            dto.getId(), facilityId, programId, false);

        if (requisitions.size() > 0) {
          previousStatus = requisitions.get(0).getStatus();
        } else {
          if (null != previousStatus && previousStatus.isPreAuthorize()) {
            throw new ValidationMessageException(new Message(ERROR_FINISH_PROVIOUS_REQUISITION));
          }

          result = dto;
          break;
        }
      }
    }

    return result;
  }

}
