package org.openlmis.requisition.domain;

import org.openlmis.requisition.dto.CommentDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.RequisitionCommentService;
import org.openlmis.requisition.service.RequisitionLineCalculationService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class RequisitionDtoBuilder {

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private RequisitionLineCalculationService requisitionLineCalculationService;

  @Autowired
  private RequisitionCommentService requisitionCommentService;

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto}
   * @return new instance of {@link RequisitionDto}
   */
  public RequisitionDto build(Requisition requisition) {
    RequisitionDto requisitionDto = new RequisitionDto();

    requisition.export(requisitionDto);
    List<RequisitionLineItemDto> requisitionLineItemDtoList =
        requisitionLineCalculationService.exportToDtos(requisition.getRequisitionLineItems());
    List<CommentDto> commentDtoList =
        requisitionCommentService.exportToDtos(requisition.getComments());


    requisitionDto.setFacility(facilityReferenceDataService.findOne(requisition.getFacilityId()));
    requisitionDto.setProgram(programReferenceDataService.findOne(requisition.getProgramId()));
    requisitionDto.setProcessingPeriod(periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId()));
    requisitionDto.setRequisitionLineItems(requisitionLineItemDtoList);
    requisitionDto.setComments(commentDtoList);

    return requisitionDto;
  }

}
