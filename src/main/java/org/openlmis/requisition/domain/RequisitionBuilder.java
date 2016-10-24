package org.openlmis.requisition.domain;

import org.openlmis.requisition.exception.RequisitionInitializationException;

import java.util.ArrayList;
import java.util.UUID;

public final class RequisitionBuilder {


  private RequisitionBuilder() {
    
  }

  /**
   * Create a new instance of Requisition with given program and facility IDs and emergency flag.
   *
   * @param programId  UUID of program
   * @param facilityId UUID of facility
   * @param emergency  flag
   * @return a new instance of Requisition
   * @throws RequisitionInitializationException if any of arguments is {@code null}
   */
  public static Requisition newRequisition(UUID programId, UUID facilityId, Boolean emergency)
      throws RequisitionInitializationException {
    if (facilityId == null || programId == null || emergency == null) {
      throw new RequisitionInitializationException(
          "Requisition cannot be initiated with null id"
      );
    }

    Requisition requisition = new Requisition();
    requisition.setEmergency(emergency);
    requisition.setFacilityId(facilityId);
    requisition.setProgramId(programId);

    return requisition;
  }

  /**
   * Creates new requisition object based on data from {@link Requisition.Importer}
   *
   * @param importer instance of {@link Requisition.Importer}
   * @return new instance of requisition.
   */
  public static Requisition newRequisition(Requisition.Importer importer) {
    Requisition requisition = new Requisition();
    requisition.setId(importer.getId());
    requisition.setCreatedDate(importer.getCreatedDate());

    if (importer.getFacility() != null) {
      requisition.setFacilityId(importer.getFacility().getId());
    }
    if (importer.getProgram() != null) {
      requisition.setProgramId(importer.getProgram().getId());
    }
    if (importer.getProcessingPeriod() != null) {
      requisition.setProcessingPeriodId(importer.getProcessingPeriod().getId());
    }
    requisition.setStatus(importer.getStatus());
    requisition.setEmergency(importer.getEmergency());
    requisition.setSupplyingFacilityId(importer.getSupplyingFacility());
    requisition.setSupervisoryNodeId(importer.getSupervisoryNode());
    requisition.setRequisitionLineItems(new ArrayList<>());
    requisition.setComments(new ArrayList<>());

    if (importer.getRequisitionLineItems() != null) {
      for (RequisitionLineItem.Importer requisitionLineItem : importer.getRequisitionLineItems()) {
        requisition.getRequisitionLineItems().add(
            RequisitionLineItem.newRequisitionLineItem(requisitionLineItem)
        );
      }
    }

    if (importer.getComments() != null) {
      for (Comment.Importer comment : importer.getComments()) {
        requisition.getComments().add(Comment.newComment(comment));
      }
    }

    return requisition;
  }
}
