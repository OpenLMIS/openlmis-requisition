package org.openlmis.requisition.web;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;
import org.joda.money.Money;
import org.openlmis.requisition.domain.AuditLogEntry;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RequisitionReportDtoBuilder {

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  RequisitionReportDto build(Requisition requisition) {
    UUID id = requisition.getId();

    List<RequisitionLineItem> fullSupply = requisitionService.getFullSupplyItems(id)
        .stream().filter(l -> !l.getSkipped()).collect(Collectors.toList());
    List<RequisitionLineItem> nonFullSupply = requisitionService.getNonFullSupplyItems(id)
        .stream().filter(l -> !l.getSkipped()).collect(Collectors.toList());

    Money fullSupplyTotalCost = requisitionService.calculateTotalCost(fullSupply);
    Money nonFullSupplyTotalCost = requisitionService.calculateTotalCost(nonFullSupply);

    RequisitionReportDto reportDto = new RequisitionReportDto();
    reportDto.setRequisition(requisitionDtoBuilder.build(requisition));
    reportDto.setFullSupply(requisitionExportHelper.exportToDtos(fullSupply));
    reportDto.setNonFullSupply(requisitionExportHelper.exportToDtos(nonFullSupply));
    reportDto.setFullSupplyTotalCost(fullSupplyTotalCost);
    reportDto.setNonFullSupplyTotalCost(nonFullSupplyTotalCost);
    reportDto.setTotalCost(fullSupplyTotalCost.plus(nonFullSupplyTotalCost));

    Map<String, AuditLogEntry> statusChanges = requisition.getStatusChanges();
    if (statusChanges != null) {
      AuditLogEntry initiatedEntry = 
          statusChanges.get(RequisitionStatus.INITIATED.toString()) != null
          ? statusChanges.get(RequisitionStatus.INITIATED.toString()) : null;
      if (Objects.nonNull(initiatedEntry)) {
        reportDto.setInitiatedBy(userReferenceDataService.findOne(initiatedEntry.getAuthorId()));
        reportDto.setInitiatedDate(initiatedEntry.getChangeDate());
      }

      AuditLogEntry submittedEntry = 
          statusChanges.get(RequisitionStatus.SUBMITTED.toString()) != null
          ? statusChanges.get(RequisitionStatus.SUBMITTED.toString()) : null;
      if (Objects.nonNull(submittedEntry)) {
        reportDto.setSubmittedBy(userReferenceDataService.findOne(submittedEntry.getAuthorId()));
        reportDto.setSubmittedDate(submittedEntry.getChangeDate());
      }

      AuditLogEntry authorizedEntry = 
          statusChanges.get(RequisitionStatus.AUTHORIZED.toString()) != null
          ? statusChanges.get(RequisitionStatus.AUTHORIZED.toString()) : null;
      if (Objects.nonNull(authorizedEntry)) {
        reportDto.setAuthorizedBy(userReferenceDataService.findOne(authorizedEntry.getAuthorId()));
        reportDto.setAuthorizedDate(authorizedEntry.getChangeDate());
      }
    }

    return reportDto;
  }
}
