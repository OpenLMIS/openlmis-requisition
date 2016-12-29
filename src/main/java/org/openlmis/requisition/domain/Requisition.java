package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Type;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.web.RequisitionController;
import org.openlmis.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static org.openlmis.requisition.domain.RequisitionLineItem.ADJUSTED_CONSUMPTION;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_COLUMN;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;
import static org.springframework.util.CollectionUtils.isEmpty;

@SuppressWarnings("PMD.TooManyMethods")
@Entity
@Table(name = "requisitions")
@NoArgsConstructor
@JsonIdentityInfo(generator = ObjectIdGenerators.PropertyGenerator.class, property = "id")
public class Requisition extends BaseTimestampedEntity {

  public static final String FACILITY_ID = "facilityId";
  public static final String PROGRAM_ID = "programId";
  public static final String PROCESSING_PERIOD_ID = "processingPeriodId";
  public static final String EMERGENCY = "emergency";

  private static final String UUID = "pg-uuid";

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionController.class);
  private static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  private static final String STOCK_ON_HAND = "stockOnHand";

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

  @OneToMany(
      mappedBy = "requisition",
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      orphanRemoval = true)
  @Getter
  @Setter
  private List<Comment> comments;

  @ManyToOne
  @JoinColumn(name = "templateId", nullable = false)
  @Getter
  @Setter
  private RequisitionTemplate template;

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

  @Column(nullable = true)
  @Getter
  @Setter
  private Integer months;

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID supervisoryNodeId;

  /**
   * Constructor.
   *
   * @param facilityId id of the Facility
   * @param programId id of the Program
   * @param processingPeriodId id of the ProcessingPeriod
   * @param status status of the Requisition
   * @param emergency whether this Requisition is emergency
   */
  public Requisition(UUID facilityId, UUID programId, UUID processingPeriodId,
                     RequisitionStatus status, Boolean emergency) {
    this.facilityId = facilityId;
    this.programId = programId;
    this.processingPeriodId = processingPeriodId;
    this.status = status;
    this.emergency = emergency;
  }

  /**
   * Copy values of attributes into new or updated Requisition.
   *
   * @param requisition Requisition with new values.
   * @param stockAdjustmentReasons Collection of stockAdjustmentReasons.
   */
  public void updateFrom(
      Requisition requisition, Collection<StockAdjustmentReasonDto> stockAdjustmentReasons) {
    if (null == this.comments) {
      this.comments = new ArrayList<>();
    }

    this.comments.clear();

    if (!isEmpty(requisition.getComments())) {
      this.comments.addAll(requisition.getComments());
    }

    this.supervisoryNodeId = requisition.getSupervisoryNodeId();

    updateReqLines(requisition.getRequisitionLineItems());
    calculateTemplateFields(this.template, stockAdjustmentReasons);
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

      if (template.isColumnDisplayed(TOTAL_COLUMN)) {
        forEachLine(line -> line.setTotal(
            LineItemFieldsCalculator.calculateTotal(line)));
      }

      if (template.isColumnOnTemplate(ADJUSTED_CONSUMPTION)) {
        forEachLine(line -> {
          int adjustedConsumption =
              LineItemFieldsCalculator.calculateAdjustedConsumption(line, getMonths());
          if (adjustedConsumption != line.getAdjustedConsumption()) {
            throw new ValidationMessageException(
                new Message("requisition.error.calculate.passed-adjusted-consumption-incorrect"));
          }
        });
      }

    } catch (RequisitionTemplateColumnException ex) {
      LOGGER.warn(ex.getMessage());
    }
  }

  private void updateReqLines(Collection<RequisitionLineItem> newLineItems) {
    if (null == newLineItems) {
      return;
    }

    if (null == requisitionLineItems) {
      requisitionLineItems = new ArrayList<>();
    }

    List<RequisitionLineItem> updatedList = new ArrayList<>();

    for (RequisitionLineItem item : newLineItems) {
      RequisitionLineItem existing = requisitionLineItems
          .stream()
          .filter(l -> l.getId().equals(item.getId()))
          .findFirst().orElse(null);

      if (null == existing) {
        if (item.isNonFullSupply()) {
          item.setRequisition(this);
          updatedList.add(item);
        }
      } else {
        existing.setRequisition(this);
        existing.updateFrom(item);
        updatedList.add(existing);
      }
    }

    // is there a full supply line that is not in update list
    // it should be added. Those lines should not be removed
    // during update. Only non full supply lines can be
    // added/updated/removed.
    requisitionLineItems
        .stream()
        .filter(line -> !line.isNonFullSupply())
        .filter(line -> updatedList
            .stream()
            .map(BaseEntity::getId)
            .noneMatch(id -> Objects.equals(id, line.getId()))
        )
        .forEach(updatedList::add);

    requisitionLineItems.clear();
    requisitionLineItems.addAll(updatedList);
  }

  /**
   * Submits given requisition.
   *
   * @param orderableProducts list of orderable products from referencedata
   */
  public void submit(Collection<OrderableProductDto> orderableProducts)
      throws RequisitionException, RequisitionTemplateColumnException {
    if (!INITIATED.equals(status)) {
      throw new InvalidRequisitionStatusException("Cannot submit requisition: " + getId()
          + ", requisition must have status 'INITIATED' to be submitted.");
    }

    if (areFieldsNotFilled(template)) {
      throw new InvalidRequisitionStatusException("Cannot submit requisition: " + getId()
          + ", requisition fields must have values.");
    }
    calculatePacksToShip(orderableProducts);
    calculateAdjustedConsumption();
    status = RequisitionStatus.SUBMITTED;
  }

  /**
   * Authorize given Requisition.
   *
   * @param orderableProducts list of orderable products from referencedata
   */
  public void authorize(Collection<OrderableProductDto> orderableProducts)
      throws RequisitionException {
    if (!RequisitionStatus.SUBMITTED.equals(status)) {
      throw new InvalidRequisitionStatusException("Cannot authorize requisition: " + getId()
          + ", requisition must have status 'SUBMITTED' to be authorized.");
    }
    calculatePacksToShip(orderableProducts);
    calculateAdjustedConsumption();
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
   * Approves given requisition.
   *
   * @param orderableProducts list of orderable products from referencedata
   */
  public void approve(Collection<OrderableProductDto> orderableProducts) {
    calculatePacksToShip(orderableProducts);
    calculateAdjustedConsumption();
    status = RequisitionStatus.APPROVED;
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

  /**
   * Initiates the state of a requisition by creating line items based on products
   * @param template the requisition template for this requisition to use (based on program)
   * @param products the full supply products for this requisitions facility to build
   *                 requisition lines for
   * @param previousRequisition the previous requisition for this program/facility.
   *                            Used for field calculations.Pass null if there are no
   *                            previous requisitions.
   * @throws RequisitionTemplateColumnException if there are issues with template definitions.
   */
  public void initiate(RequisitionTemplate template,
                       Collection<ApprovedProductDto> products,
                       Requisition previousRequisition) throws RequisitionTemplateColumnException {
    this.template = template;

    setRequisitionLineItems(
        products
            .stream()
            .map(ftap -> new RequisitionLineItem(this, ftap))
            .collect(Collectors.toList())
    );

    forEachLine(line -> {
      if (null == line.getBeginningBalance()) {
        // Firstly, we set the Beginning Balance to zero for all lines.
        line.setBeginningBalance(0);
        // same for total received quantity
        line.setTotalReceivedQuantity(0);
      }
    });

    // Secondly, if we display the column ...
    // ... and if the previous requisition exists ...
    if (template.isColumnDisplayed(RequisitionLineItem.BEGINNING_BALANCE)
            && null != previousRequisition) {
      // .. for each line from the current requisition ...
      forEachLine(currentLine -> {
        // ... we try to find line in the previous requisition for the same product ...
        RequisitionLineItem previousLine = previousRequisition
                .findLineByProductId(currentLine.getOrderableProductId());

        // ... and in the end we use it to calculate beginning balance in a new line.
        currentLine.setBeginningBalance(
                LineItemFieldsCalculator.calculateBeginningBalance(previousLine));
      });
    }
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
   * Filter out requisitionLineItems that are skipped.
   *
   * @return requisitionLineItems that are not skipped
   */
  @JsonIgnore
  public List<RequisitionLineItem> getNonSkippedRequisitionLineItems() {
    return this.requisitionLineItems.stream()
        .filter(line -> !line.getSkipped())
        .collect(Collectors.toList());
  }

  /**
   * Filter out requisitionLineItems that are not skipped.
   *
   * @return requisitionLineItems that are skipped
   */
  @JsonIgnore
  public List<RequisitionLineItem> getSkippedRequisitionLineItems() {
    return this.requisitionLineItems.stream()
        .filter(RequisitionLineItem::getSkipped)
        .collect(Collectors.toList());
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Requisition.Exporter exporter) {
    exporter.setId(id);
    exporter.setCreatedDate(getCreatedDate());
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

    void setTemplate(UUID template);
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

    UUID getTemplate();
  }

  protected void calculatePacksToShip(Collection<OrderableProductDto> orderableProducts) {
    forEachLine(line -> line.calculatePacksToShip(
        orderableProducts
            .stream()
            .filter(product -> product.getId().equals(line.getOrderableProductId()))
            .findFirst()
            .orElse(null)
    ));
  }

  private void calculateAdjustedConsumption() {
    forEachLine(line -> line.setAdjustedConsumption(
        LineItemFieldsCalculator.calculateAdjustedConsumption(line, getMonths())
    ));
  }
}
