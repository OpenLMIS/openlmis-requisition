package org.openlmis.requisition.web;

import org.joda.money.Money;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

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

    if (Objects.nonNull(requisition.getCreatorId())) {
      reportDto.setCreatedBy(userReferenceDataService.findOne(requisition.getCreatorId()));
    }

    if (Objects.nonNull(requisition.getSubmitterId())) {
      reportDto.setSubmittedBy(userReferenceDataService.findOne(requisition.getSubmitterId()));
    }

    if (Objects.nonNull(requisition.getAuthorizerId())) {
      reportDto.setAuthorizedBy(userReferenceDataService.findOne(requisition.getAuthorizerId()));
    }

    return reportDto;
  }
}
