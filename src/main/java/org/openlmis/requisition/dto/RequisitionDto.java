package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
public class RequisitionDto implements Requisition.Importer, Requisition.Exporter {
  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private LocalDateTime createdDate;

  @Getter
  @Setter
  private UUID creatorId;

  @Setter
  private List<RequisitionLineItemDto> requisitionLineItems;

  @Getter
  @Setter
  private String draftStatusMessage;

  @Getter
  @Setter
  private FacilityDto facility;

  @Getter
  @Setter
  private ProgramDto program;

  @Getter
  @Setter
  private ProcessingPeriodDto processingPeriod;

  @Getter
  @Setter
  private RequisitionStatus status;

  @Getter
  @Setter
  private Boolean emergency;

  @Getter
  @Setter
  private UUID supplyingFacility;

  @Getter
  @Setter
  private UUID supervisoryNode;

  @Getter
  @Setter
  private RequisitionTemplate template;

  @Getter
  @Setter
  private List<Requisition> previousRequisitions;

  @Getter
  @Setter
  private Set<OrderableProductDto> availableNonFullSupplyProducts;

  @Override
  public List<RequisitionLineItem.Importer> getRequisitionLineItems() {
    return new ArrayList<>(
        Optional.ofNullable(requisitionLineItems).orElse(Collections.emptyList())
    );
  }
}
