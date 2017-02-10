package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;

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
