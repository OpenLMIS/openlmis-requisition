package org.openlmis.requisition.domain;

import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Type;
import org.openlmis.fulfillment.utils.LocalDateTimePersistenceConverter;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.web.RequisitionController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;

@Entity
@Table(name = "requisitions")
@NoArgsConstructor
@JsonIdentityInfo(generator = ObjectIdGenerators.PropertyGenerator.class, property = "id")
public class Requisition extends BaseEntity {

  private static final String UUID = "pg-uuid";

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionController.class);
  private static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  private static final String STOCK_ON_HAND = "stockOnHand";

  @JsonSerialize(using = LocalDateTimeSerializer.class)
  @JsonDeserialize(using = LocalDateTimeDeserializer.class)
  @Convert(converter = LocalDateTimePersistenceConverter.class)
  @Getter
  @Setter
  private LocalDateTime createdDate;

  // TODO: determine why it has to be set explicitly
  @OneToMany(
      mappedBy = "requisition",
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER,
      orphanRemoval = true)
  @Fetch(FetchMode.SELECT)
  @Getter
  @Setter
  private List<RequisitionLineItem> requisitionLineItems;

  @OneToMany(mappedBy = "requisition", cascade = CascadeType.REMOVE)
  @Getter
  @Setter
  private List<Comment> comments;

  @Column(nullable = false)
  @Getter
  @Setter
  @Type(type = UUID)
  private UUID facilityId;

  @Column(nullable = false)
  @Getter
  @Setter
  @Type(type = UUID)
  private UUID programId;

  @Column(nullable = false)
  @Getter
  @Setter
  @Type(type = UUID)
  private UUID processingPeriodId;

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID supplyingFacilityId;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  @Getter
  @Setter
  private RequisitionStatus status;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean emergency;

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID supervisoryNodeId;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
  }

  /**
   * Copy values of attributes into new or updated Requisition.
   *
   * @param requisition         Requisition with new values.
   * @param template Requisition template
   */
  public void updateFrom(Requisition requisition, RequisitionTemplate template,
                         Collection<StockAdjustmentReasonDto> stockAdjustmentReasons) {
    this.comments = requisition.getComments();
    this.supervisoryNodeId = requisition.getSupervisoryNodeId();

    updateReqLines(requisition.getRequisitionLineItems());
    calculateTemplateFields(template, stockAdjustmentReasons);
  }

  private void calculateTemplateFields(RequisitionTemplate template,
                                        Collection<StockAdjustmentReasonDto>
                                            stockAdjustmentReasons) {
    try {
      forEachLine(line ->
          line.setTotalLossesAndAdjustments(
              LineItemFieldsCalculator.calculateTotalLossesAndAdjustments(
                  line, stockAdjustmentReasons)));

      if (template.isColumnDisplayed(STOCK_ON_HAND)) {
        if (template.isColumnCalculated(STOCK_ON_HAND)) {
          forEachLine(line ->  line.setStockOnHand(
              LineItemFieldsCalculator.calculateStockOnHand(line)));
        }
      } else {
        forEachLine(line -> line.setStockOnHand(null));
      }

      if (template.isColumnDisplayed(TOTAL_CONSUMED_QUANTITY)) {
        if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)) {
          forEachLine(line -> line.setTotalConsumedQuantity(
              LineItemFieldsCalculator.calculateTotalConsumedQuantity(line)));
        }
      } else {
        forEachLine(line -> line.setTotalConsumedQuantity(null));
      }

      if (template.isColumnDisplayed(TOTAL)) {
        forEachLine(line -> line.setTotal(
            LineItemFieldsCalculator.calculateTotal(line)));
      }

    } catch (RequisitionTemplateColumnException ex) {
      LOGGER.warn(ex.getMessage());
    }
  }

  private void updateReqLines(Collection<RequisitionLineItem> lineItems) {
    if (null == requisitionLineItems) {
      this.requisitionLineItems = new ArrayList<>();
    }

    if (lineItems != null) {
      for (RequisitionLineItem existing : requisitionLineItems) {
        RequisitionLineItem item = lineItems
                .stream()
                .filter(l -> l.getId().equals(existing.getId()))
                .findFirst().orElse(null);

        if (null != item) {
          existing.setRequisition(this);
          existing.updateFrom(item);
        }
      }
    }
  }

  /**
   * Submits given requisition.
   *
   * @param template Requisition template
   */
  public void submit(RequisitionTemplate template)
      throws RequisitionException, RequisitionTemplateColumnException {
    if (!INITIATED.equals(status)) {
      throw new InvalidRequisitionStatusException("Cannot submit requisition: " + getId()
          + ", requisition must have status 'INITIATED' to be submitted.");
    }

    if (areFieldsNotFilled(template)) {
      throw new InvalidRequisitionStatusException("Cannot submit requisition: " + getId()
          + ", requisition fields must have values.");
    }

    status = RequisitionStatus.SUBMITTED;
  }

  /**
   * Authorize given Requisition.
   */
  public void authorize() throws RequisitionException {
    if (!RequisitionStatus.SUBMITTED.equals(status)) {
      throw new InvalidRequisitionStatusException("Cannot authorize requisition: " + getId()
          + ", requisition must have status 'SUBMITTED' to be authorized.");
    }

    status = RequisitionStatus.AUTHORIZED;
  }

  private boolean areFieldsNotFilled(RequisitionTemplate template)
      throws RequisitionTemplateColumnException {
    if (null == requisitionLineItems) {
      return false;
    }

    boolean isTotalConsumedQuantityCalculated =
        template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY);
    boolean isStockOnHandCalculated =
        template.isColumnCalculated(STOCK_ON_HAND);

    for (RequisitionLineItem line : requisitionLineItems) {
      if (isTotalConsumedQuantityCalculated
          && line.allRequiredCalcFieldsNotFilled(TOTAL_CONSUMED_QUANTITY)) {
        return true;
      }

      if (isStockOnHandCalculated
          && line.allRequiredCalcFieldsNotFilled(STOCK_ON_HAND)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Finds first RequisitionLineItem that have productId property equals to the given productId
   * argument.
   *
   * @param productId UUID of orderable product
   * @return first RequisitionLineItem that have productId property equals to the given productId
   *         argument; otherwise null;
   */
  public RequisitionLineItem findLineByProductId(UUID productId) {
    if (null == requisitionLineItems) {
      return null;
    }

    return requisitionLineItems
        .stream()
        .filter(e -> Objects.equals(productId, e.getOrderableProductId()))
        .findFirst()
        .orElse(null);
  }

  public void forEachLine(Consumer<RequisitionLineItem> consumer) {
    Optional.ofNullable(requisitionLineItems)
        .ifPresent(list -> list.forEach(consumer));
  }

  @JsonIgnore
  public boolean isPreAuthorize() {
    return status.isPreAuthorize();
  }

  @JsonIgnore
  public boolean isPostSubmitted() {
    return status.isPostSubmitted();
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Requisition.Exporter exporter) {
    exporter.setId(id);
    exporter.setCreatedDate(createdDate);
    exporter.setStatus(status);
    exporter.setEmergency(emergency);
    exporter.setSupplyingFacility(supplyingFacilityId);
    exporter.setSupervisoryNode(supervisoryNodeId);
  }

  public interface Exporter {
    void setId(UUID id);

    void setCreatedDate(LocalDateTime createdDate);

    void setStatus(RequisitionStatus status);

    void setEmergency(Boolean emergency);

    void setSupplyingFacility(UUID supplyingFacility);

    void setSupervisoryNode(UUID supervisoryNode);
  }

  public interface Importer {
    UUID getId();

    LocalDateTime getCreatedDate();

    List<RequisitionLineItem.Importer> getRequisitionLineItems();

    List<Comment.Importer> getComments();

    FacilityDto getFacility();

    ProgramDto getProgram();

    ProcessingPeriodDto getProcessingPeriod();

    RequisitionStatus getStatus();

    Boolean getEmergency();

    UUID getSupplyingFacility();

    UUID getSupervisoryNode();
  }

}
