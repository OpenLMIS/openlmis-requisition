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
