package org.openlmis.requisition.domain;

import static org.apache.commons.lang.BooleanUtils.isFalse;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NULL_ID;

import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.utils.Message;

import java.util.ArrayList;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

public final class RequisitionBuilder {

  private RequisitionBuilder() {
  }

  /**
   * Create a new instance of Requisition with given facility, program and creator IDs
   * and emergency flag.
   *
   * @param facilityId UUID of facility
   * @param programId  UUID of program
   * @param creatorId  UUID of creator
   * @param emergency  flag
   * @return a new instance of Requisition
   * @throws ValidationMessageException if any of arguments is {@code null}
   */
  public static Requisition newRequisition(UUID facilityId, UUID programId, UUID creatorId,
                                           Boolean emergency) {
    if (facilityId == null || programId == null || creatorId == null || emergency == null) {
      throw new ValidationMessageException(new Message(ERROR_NULL_ID));
    }
    return new Requisition(facilityId, programId, null, creatorId, null, emergency);
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
          new Message("requisition.error.processingPeriod.null"));
    }
    UUID processingPeriodId = importer.getProcessingPeriod().getId();

    Requisition requisition = new Requisition(facilityId, programId, processingPeriodId,
        importer.getCreatorId(), importer.getStatus(), importer.getEmergency());
    requisition.setId(importer.getId());
    requisition.setCreatedDate(importer.getCreatedDate());

    requisition.setSubmittedDate(importer.getSubmittedDate());
    requisition.setSubmitterId(importer.getSubmitterId());
    requisition.setAuthorizedDate(importer.getAuthorizedDate());
    requisition.setAuthorizerId(importer.getAuthorizerId());

    requisition.setSupplyingFacilityId(importer.getSupplyingFacility());
    requisition.setSupervisoryNodeId(importer.getSupervisoryNode());
    requisition.setRequisitionLineItems(new ArrayList<>());
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

    requisition.setDraftStatusMessage(importer.getDraftStatusMessage());
    requisition.setPreviousRequisitions(importer.getPreviousRequisitions());

    if (null != importer.getAvailableNonFullSupplyProducts()) {
      requisition.setAvailableNonFullSupplyProducts(
          importer.getAvailableNonFullSupplyProducts()
          .stream()
          .map(OrderableProductDto::getId)
          .collect(Collectors.toSet())
      );
    }

    return requisition;
  }

  private static boolean isSkipped(RequisitionLineItem.Importer requisitionLineItem) {
    return requisitionLineItem.getSkipped() != null && requisitionLineItem.getSkipped();
  }
}
