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

package org.openlmis.requisition.domain;

import static org.apache.commons.lang.BooleanUtils.isFalse;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NULL_ID;

import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
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
   * Create a new instance of Requisition with given facility, program IDs and emergency flag.
   *
   * @param facilityId UUID of facility
   * @param programId  UUID of program
   * @param emergency  flag
   * @return a new instance of Requisition
   * @throws ValidationMessageException if any of arguments is {@code null}
   */
  public static Requisition newRequisition(UUID facilityId, UUID programId, Boolean emergency) {
    if (facilityId == null || programId == null || emergency == null) {
      throw new ValidationMessageException(new Message(ERROR_NULL_ID));
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
          new Message("requisition.error.processingPeriod.null"));
    }
    UUID processingPeriodId = importer.getProcessingPeriod().getId();

    Requisition requisition = new Requisition(facilityId, programId, processingPeriodId,
        importer.getStatus(), importer.getEmergency());
    requisition.setId(importer.getId());
    requisition.setCreatedDate(importer.getCreatedDate());
    requisition.setModifiedDate(importer.getModifiedDate());

    requisition.setSupplyingFacilityId(importer.getSupplyingFacility());
    requisition.setSupervisoryNodeId(importer.getSupervisoryNode());
    requisition.setRequisitionLineItems(new ArrayList<>());
    requisition.setNumberOfMonthsInPeriod(importer.getProcessingPeriod().getDurationInMonths());

    if (importer.getRequisitionLineItems() != null) {
      for (RequisitionLineItem.Importer requisitionLineItem : importer.getRequisitionLineItems()) {
        Optional<ProgramOrderableDto> program = requisitionLineItem
            .getOrderable()
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
          .map(OrderableDto::getId)
          .collect(Collectors.toSet())
      );
    }

    return requisition;
  }

  private static boolean isSkipped(RequisitionLineItem.Importer requisitionLineItem) {
    return requisitionLineItem.getSkipped() != null && requisitionLineItem.getSkipped();
  }
}
