package org.openlmis.requisition.domain;

import static org.apache.commons.lang.BooleanUtils.isFalse;

import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.utils.Message;

import java.util.ArrayList;
import java.util.Optional;
import java.util.UUID;

public final class RequisitionBuilder {

  private RequisitionBuilder() {
  }

  /**
   * Create a new instance of Requisition with given facility and program IDs and emergency flag.
   *
   * @param facilityId UUID of facility
   * @param programId  UUID of program
   * @param emergency  flag
   * @return a new instance of Requisition
   * @throws ValidationMessageException if any of arguments is {@code null}
   */
  public static Requisition newRequisition(UUID facilityId, UUID programId, Boolean emergency) {
    if (facilityId == null || programId == null || emergency == null) {
      throw new ValidationMessageException(new Message("requisition.error.initiate.null-id"));
    }
    return new Requisition(facilityId, programId, null, null, emergency);
  }

  /**
   * Creates new requisition object based on data from {@link Requisition.Importer}
   *
   * @param importer instance of {@link Requisition.Importer}
   * @return new instance of requisition.
   */
  public static Requisition newRequisition(Requisition.Importer importer,
                                           RequisitionTemplate template) {
    UUID facilityId = null;
    UUID programId = null;
    if (importer.getFacility() != null) {
      facilityId = importer.getFacility().getId();
    }
    if (importer.getProgram() != null) {
      programId = importer.getProgram().getId();
    }
    if (importer.getProcessingPeriod() == null) {
      throw new ValidationMessageException(
          new Message("requisition.error.processing-period-cannot-be-null"));
    }
    UUID processingPeriodId = importer.getProcessingPeriod().getId();

    Requisition requisition = new Requisition(facilityId, programId, processingPeriodId,
        importer.getStatus(), importer.getEmergency());
    requisition.setId(importer.getId());
    requisition.setCreatedDate(importer.getCreatedDate());
    requisition.setCreatorId(importer.getCreatorId());

    requisition.setSupplyingFacilityId(importer.getSupplyingFacility());
    requisition.setSupervisoryNodeId(importer.getSupervisoryNode());
    requisition.setRequisitionLineItems(new ArrayList<>());
    requisition.setComments(new ArrayList<>());
    requisition.setNumberOfMonthsInPeriod(importer.getProcessingPeriod().getDurationInMonths());

    if (importer.getRequisitionLineItems() != null) {
      for (RequisitionLineItem.Importer requisitionLineItem : importer.getRequisitionLineItems()) {
        Optional<ProductDto> program = requisitionLineItem
            .getOrderableProduct()
            .getPrograms()
            .stream()
            .filter(e -> requisition.getProgramId().equals(e.getProgramId()))
            .findFirst();

        RequisitionLineItem item = RequisitionLineItem.newRequisitionLineItem(requisitionLineItem);
        program.ifPresent(p -> item.setNonFullSupply(isFalse(p.getFullSupply())));

        if (isSkipped(requisitionLineItem) && importer.getStatus().isPreAuthorize()) {
          item.skipLineItem(template);
        }
        requisition.getRequisitionLineItems().add(item);
      }
    }

    if (importer.getComments() != null) {
      for (Comment.Importer comment : importer.getComments()) {
        requisition.getComments().add(Comment.newComment(comment));
      }
    }
    
    requisition.setDraftStatusMessage(importer.getDraftStatusMessage());

    return requisition;
  }

  private static boolean isSkipped(RequisitionLineItem.Importer requisitionLineItem) {
    return requisitionLineItem.getSkipped() != null && requisitionLineItem.getSkipped();
  }
}
