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

import static org.openlmis.requisition.dto.BasicProcessingPeriodDto.START_DATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FINISH_PROVIOUS_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_SUGGESTED_PERIOD;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PERIOD_SHOULD_BE_OLDEST_AND_NOT_ASSOCIATED;

import java.time.LocalDate;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionPeriod;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionPeriodDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.web.FacilitySupportsProgramHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

@Service
public class PeriodService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PeriodService.class);

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  public Collection<ProcessingPeriodDto> search(UUID scheduleId, LocalDate endDate) {
    return periodReferenceDataService.search(scheduleId, endDate);
  }

  public Collection<ProcessingPeriodDto> searchByProgramAndFacility(UUID programId,
      UUID facilityId) {
    return periodReferenceDataService.searchByProgramAndFacility(programId, facilityId);
  }

  public Collection<ProcessingPeriodDto> searchByProgramAndFacilityAndDateRange(UUID programId,
      UUID facilityId, LocalDate startDate, LocalDate endDate) {
    return periodReferenceDataService.search(programId, facilityId, startDate, endDate);
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

    LocalDate programStartDate = getProgramStartDate(programId, facilityId);

    return periods
            .stream()
            .filter(period -> {
              // check if period is in current date. For example if currently we have
              // 10th November 2016 then only periods that have start date before (or equal to)
              // current date and end date after (or equal to) current date should be processed.
              LocalDate currentDate = LocalDate.now();

              return !currentDate.isBefore(period.getStartDate())
                      && !currentDate.isAfter(period.getEndDate())
                      && (programStartDate == null
                      || !period.getStartDate().isBefore(programStartDate));
            })
            .collect(Collectors.toList());
  }

  /**
   * Gets a RequisitionPeriodDto list for initiate, for the given program and facility.
   *
   * @param program   UUID of Program.
   * @param facility  UUID of Facility.
   * @param emergency decide if periods should be found for standard or emergency requisitions.
   * @return a list of RequisitionPeriodDto.
   */
  public Collection<RequisitionPeriodDto> getPeriods(
      UUID program, UUID facility, boolean emergency) {
    Profiler profiler = new Profiler("PERIOD_SERVICE");
    profiler.setLogger(LOGGER);

    return emergency ? getPeriodsForEmergency(program, facility, profiler)
       : getPeriodsForNonEmergency(program, facility, profiler);
  }


  /**
   * Retrieves a list of RequisitionPeriodDto for the specified program and facility.
   * It processes non-emergency requisitions by filtering and adjusting the requisition periods
   * and combining them with the corresponding requisition data.
   *
   * @param program   UUID of the Program.
   * @param facility  UUID of the Facility.
   * @return a list of RequisitionPeriodDto objects for non-emergency requisitions.
   */
  private Collection<RequisitionPeriodDto> getPeriodsForNonEmergency(
      UUID program, UUID facility, Profiler profiler) {
    profiler.start("SEARCH_PERIODS_BY_PROGRAM_AND_FACILITY");
    Collection<ProcessingPeriodDto> periods = searchByProgramAndFacility(program, facility);

    profiler.start("RETRIEVE_REQUISITION_ID_AND_STATUS_PAIRS");
    List<RequisitionPeriod> requisitionIdStatusList = requisitionRepository
        .searchRequisitionIdAndStatusPairs(facility, program, Boolean.FALSE);

    profiler.start("BUILD_REQUISITION_PERIOD_DTOS");
    List<RequisitionPeriodDto> requisitionPeriods = periods.stream()
        .map(RequisitionPeriodDto::newInstance).collect(Collectors.toList());

    profiler.start("FILTER_POSTAUTHORIZE_REQUISITIONS");
    List<RequisitionPeriod> postAuthorizeRequisitionsPeriods = requisitionIdStatusList.stream()
        .filter(requisitionPeriod ->  !requisitionPeriod.getRequisitionStatus().isPreAuthorize())
        .collect(Collectors.toList());

    profiler.start("REMOVE_POSTAUTHORIZE_PERIODS");
    postAuthorizeRequisitionsPeriods.forEach(postAuthorizeRequisitionPeriod -> requisitionPeriods
        .stream()
        .filter(period -> period.getId().equals(postAuthorizeRequisitionPeriod.getPeriodId()))
        .findFirst()
        .ifPresent(requisitionPeriods::remove)
    );

    profiler.start("UPDATE_REQUISITION_PERIODS_WITH_ID_AND_STATUS");
    requisitionPeriods.forEach(requisitionPeriodDto -> requisitionIdStatusList
        .stream()
        .filter(requisitionPeriod ->
          requisitionPeriod.getPeriodId().equals(requisitionPeriodDto.getId()))
        .findFirst()
        .ifPresent(requisitionPeriod -> setRequisitionPeriodStatusAndId(
          requisitionPeriodDto, requisitionPeriod)
        )
    );

    profiler.stop().log();
    return requisitionPeriods;
  }

  /**
   * Gets a list of RequisitionPeriodDto for the given program and facility.
   * The method retrieves the current periods, searches for a 'preauthorized'
   * requisition periods and combines the period information to return a list
   * of requisition periods.
   *
   * @param program   UUID of Program.
   * @param facility  UUID of Facility.
   * @return a list of RequisitionPeriodDto.
   */
  private Collection<RequisitionPeriodDto> getPeriodsForEmergency(
      UUID program, UUID facility, Profiler profiler) {
    profiler.start("FETCH_CURRENT_PROCESSING_PERIODS");
    Collection<ProcessingPeriodDto> periods = getCurrentPeriods(program, facility);

    profiler.start("RETRIEVE_REQUISITION_ID_AND_STATUS_PAIRS");
    List<RequisitionPeriod> requisitionIdStatusList = requisitionRepository
        .searchRequisitionIdAndStatusPairs(facility, program, Boolean.TRUE);

    profiler.start("FILTER_AND_PROCESS_PREAUTHORIZED_REQUISITIONS");
    List<RequisitionPeriod> preAuthorizedRequisitions = requisitionIdStatusList.stream()
        .filter(requisitionPeriod -> requisitionPeriod.getRequisitionStatus().isPreAuthorize())
        .collect(Collectors.toList());

    Set<UUID> periodIds = preAuthorizedRequisitions.stream()
        .map(RequisitionPeriod::getPeriodId)
        .collect(Collectors.toSet());
    profiler.start("FETCH_PERIODS_FOR_PREAUTHORIZED_REQUISITIONS");
    List<ProcessingPeriodDto> periodList = periodReferenceDataService.search(periodIds);

    profiler.start("COMBINE_PERIODS_WITH_PREAUTHORIZED_REQUISITIONS");
    List<RequisitionPeriodDto> requisitionPeriods = periods.stream()
        .map(RequisitionPeriodDto::newInstance)
        .collect(Collectors.toList());

    profiler.start("PROCESS_PREAUTHORIZED_REQUISITION");
    preAuthorizedRequisitions.forEach(preAuthorizeRequisition -> periodList.stream()
        .filter(period -> period.getId().equals(preAuthorizeRequisition.getPeriodId()))
        .findFirst()
        .ifPresent(period -> {
          RequisitionPeriodDto additionalPeriod = RequisitionPeriodDto.newInstance(period);
          additionalPeriod.setRequisitionStatus(preAuthorizeRequisition.getRequisitionStatus());
          additionalPeriod.setRequisitionId(preAuthorizeRequisition.getRequisitionId());
          requisitionPeriods.add(additionalPeriod);
        }));

    profiler.stop().log();
    return requisitionPeriods;
  }


  /**
   * Find recent periods for the given period.
   *
   * @param periodId UUID of period
   * @param amount   of previous periods
   * @return list previous period or {@code null} if not found.
   */
  public List<ProcessingPeriodDto> findPreviousPeriods(UUID periodId, int amount) {
    ProcessingPeriodDto period = getPeriod(periodId);
    return findPreviousPeriods(period, amount);
  }

  /**
   * Finds recent periods for the given period.
   *
   * @param period processing period
   * @param amount of previous periods
   * @return previous period or {@code null} if not found.
   */
  public List<ProcessingPeriodDto> findPreviousPeriods(ProcessingPeriodDto period, int amount) {
    if (null == period) {
      return Collections.emptyList();
    }

    return periodReferenceDataService.search(
        period.getProcessingSchedule().getId(),
        period.getStartDate().minusDays(1),
        PageRequest.of(0, amount, Direction.DESC, START_DATE));
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

    if (suggestedPeriodId != null && period != null) {

      ProcessingPeriodDto
          proposedPeriod = getPeriod(suggestedPeriodId);

      if (proposedPeriod != null && !proposedPeriod.getId()
          .equals(period.getId())) {
        period = proposedPeriod;
      }

    }

    if (period == null) {
      throw new ValidationMessageException(new Message(
          ERROR_PERIOD_SHOULD_BE_OLDEST_AND_NOT_ASSOCIATED));
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

      LocalDate programStartDate = getProgramStartDate(programId, facilityId);
      if (programStartDate != null) {
        periods = periods.stream()
                .filter(p -> !p.getStartDate().isBefore(programStartDate))
                .collect(Collectors.toList());
      }
      for (ProcessingPeriodDto dto : periods) {
        // There is always maximum one regular requisition for given period, facility and program
        List<Requisition> requisitions = requisitionRepository.searchRequisitions(
            dto.getId(), facilityId, programId, false);

        if (!requisitions.isEmpty()) {
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

  private LocalDate getProgramStartDate(UUID programId, UUID facilityId) {
    SupportedProgramDto program = facilitySupportsProgramHelper.getSupportedProgram(facilityId,
            programId);
    return program.getSupportStartDate();
  }

  private void setRequisitionPeriodStatusAndId(RequisitionPeriodDto requisitionPeriodDto,
      RequisitionPeriod requisitionPeriod) {
    requisitionPeriodDto.setRequisitionId(requisitionPeriod.getRequisitionId());
    requisitionPeriodDto.setRequisitionStatus(requisitionPeriod.getRequisitionStatus());
  }

}
