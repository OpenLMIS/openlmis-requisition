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

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReportingRateReportDto;
import org.openlmis.requisition.dto.RequisitionCompletionDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.GeographicZoneReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Component
public class ReportingRateReportDtoBuilder {
  private static int DUE_DAYS = 10;
  private static int LATEST_PERIODS = 3;
  private static int GEOGRAPHIC_ZONE_LEVEL = 3;
  private static RequisitionStatus REQUIRED_STATUS = RequisitionStatus.APPROVED;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private GeographicZoneReferenceDataService geographicZoneReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  /**
   * Creates a DTO for reporting rate report based on given parameters.
   *
   * @param program program for reporting
   * @param period processing period for reporting
   * @param zone geographic zone for reporting
   * @return newly created report dto
   */
  public ReportingRateReportDto build(
      ProgramDto program, ProcessingPeriodDto period, GeographicZoneDto zone, Integer dueDays) {
    if (dueDays == null) {
      dueDays = DUE_DAYS;
    }
    ReportingRateReportDto report = new ReportingRateReportDto();

    Collection<ProcessingPeriodDto> periods = getLatestPeriods(period, LATEST_PERIODS);
    Collection<GeographicZoneDto> zones = getAvailableGeographicZones(zone);
    Collection<FacilityDto> facilities = getAvailableFacilities(zones);

    report.setCompletionByPeriod(getCompletionsByPeriod(program, periods, facilities, dueDays));
    report.setCompletionByZone(getCompletionsByZone(program, periods, zones, dueDays));

    return report;
  }

  private List<RequisitionCompletionDto> getCompletionsByPeriod(
      ProgramDto program, Collection<ProcessingPeriodDto> periods,
      Collection<FacilityDto> facilities, Integer dueDays) {
    List<RequisitionCompletionDto> completionByPeriod = new ArrayList<>();

    for (ProcessingPeriodDto period : periods) {
      RequisitionCompletionDto completion = getCompletionForFacilities(
          program, Collections.singletonList(period), facilities, dueDays);
      completion.setGrouping(period.getName());
      completionByPeriod.add(completion);
    }

    return completionByPeriod;
  }

  private List<RequisitionCompletionDto> getCompletionsByZone(
      ProgramDto program, Collection<ProcessingPeriodDto> periods,
      Collection<GeographicZoneDto> zones, Integer dueDays) {
    List<RequisitionCompletionDto> completionByZone = new ArrayList<>();

    for (GeographicZoneDto zone : zones) {
      Collection<FacilityDto> facilities = getAvailableFacilities(Collections.singletonList(zone));

      if (!facilities.isEmpty()) {
        RequisitionCompletionDto completion =
            getCompletionForFacilities(program, periods, facilities, dueDays);
        completion.setGrouping(zone.getName());
        completionByZone.add(completion);
      }
    }

    // Sort by zone names
    return completionByZone
        .stream()
        .sorted((left, right) -> left.getGrouping().compareTo(right.getGrouping()))
        .collect(Collectors.toList());
  }

  private RequisitionCompletionDto getCompletionForFacilities(
      ProgramDto program, Collection<ProcessingPeriodDto> periods,
      Collection<FacilityDto> facilities, Integer dueDays) {
    CompletionCounter completions = new CompletionCounter();

    for (ProcessingPeriodDto period : periods) {
      LocalDate dueDate = period.getEndDate().plusDays(dueDays);

      for (FacilityDto facility : facilities) {
        List<Requisition> requisitions = requisitionRepository
            .searchRequisitions(period.getId(), facility.getId(), program.getId(), false);

        updateCompletionsWithRequisitions(completions, requisitions, dueDate);
      }
    }

    int onTime = completions.getOnTime();
    int missed = completions.getMissed();
    int late = completions.getLate();
    int total = onTime + late + missed;

    if (total == 0) {
      missed = 1;
      total = 1;
    }

    RequisitionCompletionDto completion = new RequisitionCompletionDto();
    completion.setCompleted(((double)(onTime + late)) / total);
    completion.setMissed(((double)missed) / total);
    completion.setOnTime(((double)onTime) / total);
    completion.setLate(((double)late) / total);

    return completion;
  }

  void updateCompletionsWithRequisitions(
      CompletionCounter completions, List<Requisition> requisitions, LocalDate dueDate) {
    int missed = completions.getMissed();
    int late = completions.getLate();
    int onTime = completions.getOnTime();

    if (!requisitions.isEmpty()) {
      for (Requisition requisition : requisitions) {
        StatusLogEntry entry = requisition.getStatusChanges().get(REQUIRED_STATUS.toString());
        if (entry == null) {
          missed++;
        } else {
          LocalDate submissionDate = entry.getChangeDate().toLocalDate();
          if (submissionDate.isAfter(dueDate)) {
            late++;
          } else {
            onTime++;
          }
        }
      }
    } else {
      missed++;
    }

    completions.setMissed(missed);
    completions.setOnTime(onTime);
    completions.setLate(late);
  }

  Collection<FacilityDto> getAvailableFacilities(Collection<GeographicZoneDto> zones) {
    List<FacilityDto> facilities = new ArrayList<>();
    for (GeographicZoneDto zone : zones) {
      facilities.addAll(facilityReferenceDataService.search(null, null, zone.getId(), true));
    }

    return facilities
        .stream()
        .filter(FacilityDto::getActive)
        .collect(Collectors.toList());
  }

  Collection<ProcessingPeriodDto> getLatestPeriods(ProcessingPeriodDto latest, int amount) {
    List<ProcessingPeriodDto> periods = new ArrayList<>();

    if (amount > 1) {
      // Retrieve list of periods prior to latest one
      List<ProcessingPeriodDto> previousPeriods =
          periodReferenceDataService.search(latest.getProcessingSchedule().getId(), null)
              .stream()
              .filter(p -> p.getStartDate().isBefore(latest.getStartDate()))
              // Sort by date descending -> get 2 most recent
              .sorted((left, right) -> right.getStartDate().compareTo(left.getStartDate()))
              .limit(amount - 1)
              // Sort by date ascending -> return as normal
              .sorted((left, right) -> left.getStartDate().compareTo(right.getStartDate()))
              .collect(Collectors.toList());

      periods.addAll(previousPeriods);
    }

    periods.add(latest);
    return periods;
  }

  Collection<GeographicZoneDto> getAvailableGeographicZones(GeographicZoneDto zone) {
    if (zone == null) {
      return geographicZoneReferenceDataService.search(GEOGRAPHIC_ZONE_LEVEL, null);
    } else {
      return geographicZoneReferenceDataService.search(GEOGRAPHIC_ZONE_LEVEL, zone.getId());
    }
  }

  @Getter
  @Setter
  @NoArgsConstructor
  class CompletionCounter {
    int missed;
    int late;
    int onTime;
  }
}
