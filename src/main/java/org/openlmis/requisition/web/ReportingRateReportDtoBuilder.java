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

@Component
public class ReportingRateReportDtoBuilder {
  // TODO: MAKE THOSE CONFIGURABLE, OR SET AS LOCAL VARIABLES
  private static int GEOGRAPHIC_ZONE_LEVEL = 2;
  private static int DAYS_DUE = 10;
  private static int LATEST_PERIODS = 3;
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
      ProgramDto program, ProcessingPeriodDto period, GeographicZoneDto zone) {
    ReportingRateReportDto report = new ReportingRateReportDto();

    Collection<ProcessingPeriodDto> periods = getLatestPeriods(period, LATEST_PERIODS);
    Collection<GeographicZoneDto> zones = getAvailableGeographicZones(zone);
    Collection<FacilityDto> facilities = getAvailableFacilities(zones);

    report.setCompletionByPeriod(getCompletionsByPeriod(program, periods, facilities));
    report.setCompletionByZone(getCompletionsByZone(program, period, zones));

    return report;
  }

  private List<RequisitionCompletionDto> getCompletionsByPeriod(
      ProgramDto program, Collection<ProcessingPeriodDto> periods,
      Collection<FacilityDto> facilities) {
    List<RequisitionCompletionDto> completionByPeriod = new ArrayList<>();

    for (ProcessingPeriodDto period : periods) {
      LocalDate dueDate = period.getEndDate().plusDays(DAYS_DUE);
      List<Requisition> requisitions = new ArrayList<>();

      for (FacilityDto facility : facilities) {
        requisitions.addAll(requisitionRepository
            .searchRequisitions(period.getId(), facility.getId(), program.getId(), false));
      }

      RequisitionCompletionDto completion = getCompletionForRequisitions(requisitions, dueDate);
      completion.setGrouping(period.getName());
      completionByPeriod.add(completion);
    }

    return completionByPeriod;
  }

  private List<RequisitionCompletionDto> getCompletionsByZone(
      ProgramDto program, ProcessingPeriodDto period, Collection<GeographicZoneDto> zones) {
    List<RequisitionCompletionDto> completionByZone = new ArrayList<>();

    for (GeographicZoneDto zone : zones) {
      LocalDate dueDate = period.getEndDate().plusDays(DAYS_DUE);
      Collection<Requisition> requisitions = new ArrayList<>();
      Collection<FacilityDto> zoneFacilities =
          facilityReferenceDataService.search(null, null, zone.getId(), false);

      for (FacilityDto facility : zoneFacilities) {
        requisitions.addAll(requisitionRepository
            .searchRequisitions(period.getId(), facility.getId(), program.getId(), false));
      }

      RequisitionCompletionDto completion = getCompletionForRequisitions(requisitions, dueDate);
      completion.setGrouping(zone.getName());
      completionByZone.add(completion);
    }

    return completionByZone;
  }

  private RequisitionCompletionDto getCompletionForRequisitions(
      Collection<Requisition> requisitions, LocalDate dueDate) {
    RequisitionCompletionDto completion = new RequisitionCompletionDto();

    int total = requisitions.size();
    if (total > 0) {
      int late = 0;
      int missed = 0;
      int onTime = 0;

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

      completion.setCompleted(((double)(onTime + late)) / total);
      completion.setMissed(((double)missed) / total);
      completion.setOnTime(((double)onTime) / total);
      completion.setLate(((double)late) / total);
    } else {
      completion.setMissed(1);
    }

    return completion;
  }

  private Collection<FacilityDto> getAvailableFacilities(Collection<GeographicZoneDto> zones) {
    List<FacilityDto> facilities = new ArrayList<>();
    for (GeographicZoneDto zone : zones) {
      facilities.addAll(facilityReferenceDataService.search(null, null, zone.getId(), false));
    }
    return facilities;
  }

  private Collection<ProcessingPeriodDto> getLatestPeriods(ProcessingPeriodDto latest, int amount) {
    List<ProcessingPeriodDto> periods = new ArrayList<>();

    if (amount > 1) {
      // Retrieve list of periods prior to latest one
      List<ProcessingPeriodDto> previousPeriods =
          periodReferenceDataService.search(latest.getProcessingSchedule().getId(), null)
          .stream()
          .filter(p -> p.getStartDate().isBefore(latest.getStartDate()))
          .sorted((left, right) -> left.getStartDate().compareTo(right.getStartDate()))
          .limit(amount - 1)
          .collect(Collectors.toList());

      periods.addAll(previousPeriods);
    }

    periods.add(latest);
    return periods;
  }

  private Collection<GeographicZoneDto> getAvailableGeographicZones(GeographicZoneDto zone) {
    if (zone == null) {
      return geographicZoneReferenceDataService.search(GEOGRAPHIC_ZONE_LEVEL, null);
    } else {
      return Collections.singletonList(zone);
    }
  }
}
