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
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionCompletionDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Component
@SuppressWarnings("PMD.UnusedPrivateMethod")
public class ReportingRateReportDtoBuilder {

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  private RequisitionCompletionDto getCompletionForRequisitions(
      List<Requisition> requisitions, LocalDate dueDate) {
    RequisitionCompletionDto completion = new RequisitionCompletionDto();

    int total = requisitions.size();
    if (total > 0) {
      int late = 0;
      int missed = 0;
      int onTime = 0;

      for (Requisition requisition : requisitions) {
        StatusLogEntry entry = requisition.getStatusChanges()
            .get(RequisitionStatus.SUBMITTED.toString());
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
    }

    return completion;
  }

  private List<ProcessingPeriodDto> getLatestPeriods(ProcessingPeriodDto latest, int amount) {
    List<ProcessingPeriodDto> periods = Collections.singletonList(latest);

    if (amount > 1) {
      // Retrieve list of periods
      List<ProcessingPeriodDto> previousPeriods = periodReferenceDataService
          .search(latest.getProcessingSchedule().getId(), null)
          .stream()
          .filter(p -> p.getStartDate().isBefore(latest.getStartDate()))
          .sorted((left, right) -> right.getStartDate().compareTo(left.getStartDate()))
          .limit(amount - 1)
          .collect(Collectors.toList());

      periods.addAll(previousPeriods);
    }

    return periods;
  }
}
