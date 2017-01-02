package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.CommentDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.RequisitionCommentService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class RequisitionDtoBuilder {

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private RequisitionCommentService requisitionCommentService;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  /**
   * Create a list of {@link RequisitionDto} based on passed data.
   *
   * @param requisitions a list of requisitions that will be converted into DTOs.
   * @return a list of {@link RequisitionDto}
   */
  public List<RequisitionDto> build(List<Requisition> requisitions) {
    return requisitions.stream().map(this::build).collect(Collectors.toList());
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}.
   *         {@code null} if passed argument is {@code null}.
   */
  public RequisitionDto build(Requisition requisition) {
    if (null == requisition) {
      return null;
    }

    RequisitionDto requisitionDto = new RequisitionDto();

    requisition.export(requisitionDto);
    List<RequisitionLineItemDto> requisitionLineItemDtoList =
        requisitionExportHelper.exportToDtos(requisition.getRequisitionLineItems());
    List<CommentDto> commentDtoList =
        requisitionCommentService.exportToDtos(requisition.getComments());

    requisitionDto.setTemplate(requisition.getTemplate().getId());
    requisitionDto.setFacility(facilityReferenceDataService.findOne(requisition.getFacilityId()));
    requisitionDto.setProgram(programReferenceDataService.findOne(requisition.getProgramId()));
    requisitionDto.setProcessingPeriod(periodService.getPeriod(
        requisition.getProcessingPeriodId()));
    requisitionDto.setRequisitionLineItems(requisitionLineItemDtoList);
    requisitionDto.setComments(commentDtoList);

    return requisitionDto;
  }

}
