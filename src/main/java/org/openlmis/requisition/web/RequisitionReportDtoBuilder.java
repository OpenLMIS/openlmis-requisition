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

import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.openlmis.requisition.domain.AuditLogEntry;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RequisitionReportDtoBuilder {

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;
  
  /**
   * Create a {@link RequisitionReportDto} based on a given {@link Requisition}
   *
   * @param requisition a single {@link Requisition} to be converted to report dto.
   * @return a single {@link RequisitionReportDto}
   */
  public RequisitionReportDto build(Requisition requisition) {
    List<RequisitionLineItem> fullSupply =
        requisition.getNonSkippedFullSupplyRequisitionLineItems();
    List<RequisitionLineItem> nonFullSupply =
        requisition.getNonSkippedNonFullSupplyRequisitionLineItems();

    RequisitionReportDto reportDto = new RequisitionReportDto();
    reportDto.setRequisition(requisitionDtoBuilder.build(requisition));
    reportDto.setFullSupply(requisitionExportHelper.exportToDtos(fullSupply));
    reportDto.setNonFullSupply(requisitionExportHelper.exportToDtos(nonFullSupply));
    reportDto.setFullSupplyTotalCost(requisition.getFullSupplyTotalCost());
    reportDto.setNonFullSupplyTotalCost(requisition.getNonFullSupplyTotalCost());
    reportDto.setTotalCost(requisition.getTotalCost());

    Map<String, AuditLogEntry> statusChanges = requisition.getStatusChanges();
    if (statusChanges != null) {
      AuditLogEntry initiatedEntry = statusChanges.getOrDefault(
          RequisitionStatus.INITIATED.toString(), null);
      if (Objects.nonNull(initiatedEntry)) {
        reportDto.setInitiatedBy(userReferenceDataService.findOne(initiatedEntry.getAuthorId()));
        reportDto.setInitiatedDate(initiatedEntry.getChangeDate());
      }

      AuditLogEntry submittedEntry = statusChanges.getOrDefault(
          RequisitionStatus.SUBMITTED.toString(), null);
      if (Objects.nonNull(submittedEntry)) {
        reportDto.setSubmittedBy(userReferenceDataService.findOne(submittedEntry.getAuthorId()));
        reportDto.setSubmittedDate(submittedEntry.getChangeDate());
      }

      AuditLogEntry authorizedEntry = statusChanges.getOrDefault(
          RequisitionStatus.AUTHORIZED.toString(), null);
      if (Objects.nonNull(authorizedEntry)) {
        reportDto.setAuthorizedBy(userReferenceDataService.findOne(authorizedEntry.getAuthorId()));
        reportDto.setAuthorizedDate(authorizedEntry.getChangeDate());
      }
    }

    return reportDto;
  }
}
