package org.openlmis.requisition.domain;

import static org.openlmis.requisition.domain.RequisitionLineItem.ADJUSTED_CONSUMPTION;
import static org.openlmis.requisition.domain.RequisitionLineItem.AVERAGE_CONSUMPTION;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FIELD_MUST_HAVE_VALUES;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_INITIATED_TO_BE_SUBMMITED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_SUBMITTED_TO_BE_AUTHORIZED;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Type;
import org.javers.core.metamodel.annotation.DiffIgnore;
import org.javers.core.metamodel.annotation.TypeName;
import org.joda.money.CurrencyUnit;
import org.openlmis.CurrencyConfig;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.utils.Message;
import org.openlmis.utils.RequisitionHelper;

@Entity
@TypeName("Requisition")
@Table(name = "requisitions")
@NoArgsConstructor
@JsonIdentityInfo(generator = ObjectIdGenerators.PropertyGenerator.class, property = "id")
public class Requisition extends BaseTimestampedEntity {

  public static final String FACILITY_ID = "facilityId";
  public static final String PROGRAM_ID = "programId";
  public static final String PROCESSING_PERIOD_ID = "processingPeriodId";
  public static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  public static final String STOCK_ON_HAND = "stockOnHand";
  public static final String SUPERVISORY_NODE_ID = "supervisoryNodeId";
  public static final String EMERGENCY = "emergency";
  public static final String MODIFIED_DATE = "modifiedDate";

  private static final String UUID = "pg-uuid";

  @OneToMany(
      mappedBy = "requisition",
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER,
      orphanRemoval = true)
  @Fetch(FetchMode.SELECT)
  @DiffIgnore
  @Getter
  @Setter
  private List<RequisitionLineItem> requisitionLineItems;

  @Getter
  private String draftStatusMessage;

  @ManyToOne
  @JoinColumn(name = "templateId", nullable = false)
  @DiffIgnore
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

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer numberOfMonthsInPeriod;

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID supervisoryNodeId;

  @ManyToMany
  @JoinTable(name = "requisitions_previous_requisitions",
      joinColumns = { @JoinColumn(name = "requisitionId") },
      inverseJoinColumns = { @JoinColumn(name = "previousRequisitionId") })
  @DiffIgnore
  @Getter
  @Setter
  private List<Requisition> previousRequisitions;

  @ElementCollection(fetch = FetchType.EAGER, targetClass = UUID.class)
  @Column(name = "value")
  @CollectionTable(
      name = "available_non_full_supply_products",
      joinColumns = @JoinColumn(name = "requisitionId"))
  @Getter
  @Setter
  private Set<UUID> availableNonFullSupplyProducts;

  /*
   * Used to convey a small subset of the requisition-related data collected via JaVers' audit-log.
   * This is primarily used to allow users the ability to see when the RequisitionStatus changed.
   * A more holistic approach to audit logging is forthcoming.
   */
  @Transient
  @Getter
  @Setter
  private Map<String, AuditLogEntry> statusChanges;

  /**
   * Constructor.
   *
   * @param facilityId         id of the Facility
   * @param programId          id of the Program
   * @param processingPeriodId id of the ProcessingPeriod
   * @param status             status of the Requisition
   * @param emergency          whether this Requisition is emergency
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
   * @param requisition            Requisition with new values.
   * @param stockAdjustmentReasons Collection of stockAdjustmentReasons.
   */
  public void updateFrom(
      Requisition requisition, Collection<StockAdjustmentReasonDto> stockAdjustmentReasons) {

    this.numberOfMonthsInPeriod = requisition.getNumberOfMonthsInPeriod();

    this.draftStatusMessage = requisition.draftStatusMessage;

    updateReqLines(requisition.getRequisitionLineItems());
    calculateAndValidateTemplateFields(this.template, stockAdjustmentReasons);
  }

  /**
   * Initiates the state of a requisition by creating line items based on products
   *
   * @param template             the requisition template for this requisition to use (based on
   *                             program)
   * @param products             the full supply products for this requisitions facility to build
   *                             requisition lines for
   * @param previousRequisitions the previous requisitions for this program/facility. Used for field
   *                             calculations and set previous adjusted consumptions. Pass empty
   *                             list if there are no previous requisitions.
   */
  public void initiate(RequisitionTemplate template,
                       Collection<ApprovedProductDto> products,
                       List<Requisition> previousRequisitions,
                       int numberOfPreviousPeriodsToAverage) {
    this.template = template;
    this.previousRequisitions = previousRequisitions;

    setRequisitionLineItems(
        products
            .stream()
            .map(ftap -> new RequisitionLineItem(this, ftap))
            .collect(Collectors.toList())
    );

    getNonSkippedFullSupplyRequisitionLineItems().forEach(line -> {
      if (null == line.getBeginningBalance()) {
        // Firstly, we set the Beginning Balance to zero for all lines.
        line.setBeginningBalance(0);
        // same for total received quantity
        line.setTotalReceivedQuantity(0);
      }
    });

    // Secondly, if we display the column ...
    // ... and if the previous requisition exists ...
    if (!previousRequisitions.isEmpty()
        && null != previousRequisitions.get(0)
        && template.isColumnDisplayed(RequisitionLineItem.BEGINNING_BALANCE)) {
      // .. for each line from the current requisition ...
      getNonSkippedFullSupplyRequisitionLineItems().forEach(currentLine -> {
        // ... we try to find line in the previous requisition for the same product ...
        RequisitionLineItem previousLine = previousRequisitions.get(0)
            .findLineByProductId(currentLine.getOrderableId());

        // ... and in the end we use it to calculate beginning balance in a new line.
        currentLine.setBeginningBalance(
            LineItemFieldsCalculator.calculateBeginningBalance(previousLine));
      });
    }
    setPreviousAdjustedConsumptions(numberOfPreviousPeriodsToAverage);
  }

  /**
   * Submits given requisition.
   *
   * @param products orderable products that will be used by line items to update packs to ship.
   */
  public void submit(Collection<OrderableDto> products, UUID submitter) {
    RequisitionHelper.forEachLine(getNonSkippedRequisitionLineItems(),
        line -> line.updatePacksToShip(products));

    if (!INITIATED.equals(status)) {
      throw new ValidationMessageException(
          new Message(ERROR_MUST_BE_INITIATED_TO_BE_SUBMMITED, getId()));
    }

    if (RequisitionHelper.areFieldsNotFilled(template,
        getNonSkippedFullSupplyRequisitionLineItems())) {
      throw new ValidationMessageException(
          new Message(ERROR_FIELD_MUST_HAVE_VALUES, getId()));
    }

    updateConsumptionsAndTotalCost();

    status = RequisitionStatus.SUBMITTED;
  }

  /**
   * Authorize given Requisition.
   *
   * @param products orderable products that will be used by line items to update packs to ship.
   */
  public void authorize(Collection<OrderableDto> products, UUID authorizer) {
    RequisitionHelper.forEachLine(
        getNonSkippedRequisitionLineItems(), line -> line.updatePacksToShip(products));

    if (!RequisitionStatus.SUBMITTED.equals(status)) {
      throw new ValidationMessageException(
          new Message(ERROR_MUST_BE_SUBMITTED_TO_BE_AUTHORIZED, getId()));
    }

    updateConsumptionsAndTotalCost();

    status = RequisitionStatus.AUTHORIZED;
    RequisitionHelper.forEachLine(getSkippedRequisitionLineItems(), RequisitionLineItem::resetData);
  }

  /**
   * Check if the requisition is approvable.
   *
   */
  public boolean isApprovable() {
    return status == RequisitionStatus.AUTHORIZED || status == RequisitionStatus.IN_APPROVAL;
  }

  /**
   * Approves given requisition.
   *
   * @param parentNodeId supervisoryNodeDto parent node of the supervisoryNode for this requisition.
   * @param products orderable products that will be used by line items to update packs to ship.
   */
  public void approve(UUID parentNodeId, Collection<OrderableDto> products) {
    if (parentNodeId == null) {
      status = RequisitionStatus.APPROVED;
    } else {
      status = RequisitionStatus.IN_APPROVAL;
      supervisoryNodeId = parentNodeId;
    }

    RequisitionHelper.forEachLine(getNonSkippedRequisitionLineItems(),
        line -> line.updatePacksToShip(products));

    updateConsumptionsAndTotalCost();
  }

  /**
   * Finds first RequisitionLineItem that have productId property equals to the given productId
   * argument.
   *
   * @param productId UUID of orderable product
   * @return first RequisitionLineItem that have productId property equals to the given productId
   *     argument; otherwise null;
   */
  public RequisitionLineItem findLineByProductId(UUID productId) {
    if (null == requisitionLineItems) {
      return null;
    }

    return requisitionLineItems
        .stream()
        .filter(e -> Objects.equals(productId, e.getOrderableId()))
        .findFirst()
        .orElse(null);
  }

  public void setDraftStatusMessage(String draftStatusMessage) {
    this.draftStatusMessage = (draftStatusMessage == null) ? "" : draftStatusMessage;
  }

  public boolean isPreAuthorize() {
    return status.isPreAuthorize();
  }

  public boolean isPostSubmitted() {
    return status.isPostSubmitted();
  }

  /**
   * Filter out requisitionLineItems that are skipped.
   *
   * @return requisitionLineItems that are not skipped
   */
  public List<RequisitionLineItem> getNonSkippedRequisitionLineItems() {
    if (requisitionLineItems == null) {
      return Collections.emptyList();
    }
    return this.requisitionLineItems.stream()
        .filter(line -> !line.getSkipped())
        .collect(Collectors.toList());
  }

  /**
   * Filter out requisitionLineItems that are skipped and not-full supply.
   *
   * @return requisitionLineItems that are not skipped
   */
  public List<RequisitionLineItem> getNonSkippedFullSupplyRequisitionLineItems() {
    if (requisitionLineItems == null) {
      return Collections.emptyList();
    }
    return this.requisitionLineItems.stream()
        .filter(line -> !line.getSkipped())
        .filter(line -> !line.isNonFullSupply())
        .collect(Collectors.toList());
  }

  /**
   * Filter out requisitionLineItems that are not skipped.
   *
   * @return requisitionLineItems that are skipped
   */
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
    exporter.setModifiedDate(getModifiedDate());
    exporter.setStatus(status);
    exporter.setEmergency(emergency);
    exporter.setSupplyingFacility(supplyingFacilityId);
    exporter.setSupervisoryNode(supervisoryNodeId);
    exporter.setDraftStatusMessage(draftStatusMessage);
    exporter.setPreviousRequisitions(previousRequisitions);
  }

  /**
   * Sets appropriate value for Previous Adjusted Consumptions field in
   * each {@link RequisitionLineItem}.
   */
  void setPreviousAdjustedConsumptions(int numberOfPreviousPeriodsToAverage) {
    List<RequisitionLineItem> previousRequisitionLineItems = RequisitionHelper
        .getNonSkippedLineItems(previousRequisitions.subList(0, numberOfPreviousPeriodsToAverage));

    RequisitionHelper.forEachLine(requisitionLineItems,
        line -> {
          List<RequisitionLineItem> withProductId = RequisitionHelper
              .findByProductId(previousRequisitionLineItems, line.getOrderableId());
          List<Integer> adjustedConsumptions = RequisitionHelper
              .mapToAdjustedConsumptions(withProductId);

          line.setPreviousAdjustedConsumptions(adjustedConsumptions);
        });
  }

  private void calculateAndValidateTemplateFields(
      RequisitionTemplate template, Collection<StockAdjustmentReasonDto> stockAdjustmentReasons) {
    getNonSkippedFullSupplyRequisitionLineItems()
        .forEach(line -> line.calculateAndSetFields(template, stockAdjustmentReasons,
            numberOfMonthsInPeriod));
  }

  private void updateConsumptionsAndTotalCost() {
    if (template.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
      getNonSkippedFullSupplyRequisitionLineItems().forEach(line -> line.setAdjustedConsumption(
          LineItemFieldsCalculator.calculateAdjustedConsumption(line, numberOfMonthsInPeriod)
      ));
    }

    if (template.isColumnInTemplate(AVERAGE_CONSUMPTION)) {
      getNonSkippedFullSupplyRequisitionLineItems().forEach(
          RequisitionLineItem::calculateAndSetAverageConsumption);
    }

    getNonSkippedRequisitionLineItems().forEach(line -> line.setTotalCost(
        LineItemFieldsCalculator.calculateTotalCost(line,
            CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE))
    ));
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

  public interface Exporter {
    void setId(UUID id);

    void setCreatedDate(ZonedDateTime createdDate);

    void setModifiedDate(ZonedDateTime createdDate);

    void setStatus(RequisitionStatus status);

    void setEmergency(Boolean emergency);

    void setSupplyingFacility(UUID supplyingFacility);

    void setSupervisoryNode(UUID supervisoryNode);

    void setTemplate(RequisitionTemplate template);

    void setDraftStatusMessage(String draftStatusMessage);

    void setPreviousRequisitions(List<Requisition> previousRequisitions);

  }

  public interface Importer {
    UUID getId();

    ZonedDateTime getCreatedDate();

    ZonedDateTime getModifiedDate();

    List<RequisitionLineItem.Importer> getRequisitionLineItems();

    FacilityDto getFacility();

    ProgramDto getProgram();

    ProcessingPeriodDto getProcessingPeriod();

    RequisitionStatus getStatus();

    Boolean getEmergency();

    UUID getSupplyingFacility();

    UUID getSupervisoryNode();

    RequisitionTemplate getTemplate();

    String getDraftStatusMessage();

    List<Requisition> getPreviousRequisitions();

    Set<OrderableDto> getAvailableNonFullSupplyProducts();
  }
}
