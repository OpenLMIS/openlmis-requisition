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

import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateAdjustedConsumption;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateAverageConsumption;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateCalculatedOrderQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateMaximumStockQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateStockOnHand;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateTotal;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateTotalConsumedQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateTotalLossesAndAdjustments;

import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.Parameter;
import org.hibernate.annotations.Type;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.openlmis.CurrencyConfig;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

@SuppressWarnings("PMD.TooManyMethods")
@Entity
@Table(name = "requisition_line_items")
public class RequisitionLineItem extends BaseEntity {
  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionLineItem.class);

  public static final String REQUESTED_QUANTITY = "requestedQuantity";
  public static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  public static final String BEGINNING_BALANCE = "beginningBalance";
  public static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  public static final String STOCK_ON_HAND = "stockOnHand";
  public static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  public static final String TOTAL_LOSSES_AND_ADJUSTMENTS = "totalLossesAndAdjustments";
  public static final String APPROVED_QUANTITY = "approvedQuantity";
  public static final String REMARKS_COLUMN = "remarks";
  public static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";
  public static final String TOTAL_COLUMN = "total";
  static final BigDecimal PRICE_PER_PACK_IF_NULL = BigDecimal.ZERO;
  public static final String NUMBER_OF_NEW_PATIENTS_ADDED = "numberOfNewPatientsAdded";
  public static final String SKIPPED_COLUMN = "skipped";
  public static final String ADJUSTED_CONSUMPTION = "adjustedConsumption";
  public static final String AVERAGE_CONSUMPTION = "averageConsumption";
  public static final String MAXIMUM_STOCK_QUANTITY = "maximumStockQuantity";
  public static final String CALCULATED_ORDER_QUANTITY = "calculatedOrderQuantity";

  @Getter
  @Setter
  @Type(type = UUID_TYPE)
  private UUID orderableId;

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "requisitionId")
  @Getter
  @Setter
  private Requisition requisition;

  @Getter
  @Setter
  private Integer beginningBalance;

  @Getter
  @Setter
  private Integer totalReceivedQuantity;

  @Getter
  @Setter
  private Integer totalLossesAndAdjustments;

  @Getter
  @Setter
  private Integer stockOnHand;

  @Getter
  @Setter
  private Integer requestedQuantity;

  @Getter
  @Setter
  private Integer totalConsumedQuantity;

  @Getter
  @Setter
  private Integer total;

  @Getter
  @Setter
  private String requestedQuantityExplanation;

  @Column(length = 250)
  @Getter
  @Setter
  private String remarks;

  @Getter
  @Setter
  private Integer approvedQuantity;

  @Getter
  @Setter
  private Integer totalStockoutDays;

  @Getter
  @Setter
  private Long packsToShip;

  @Getter
  @Setter
  private Boolean skipped;

  @Getter
  @Setter
  @Type(type = "org.jadira.usertype.moneyandcurrency.joda.PersistentMoneyAmount",
      parameters = {@Parameter(name = "currencyCode", value = CurrencyConfig.CURRENCY_CODE)})
  private Money pricePerPack;

  @Getter
  @Setter
  @Type(type = "org.jadira.usertype.moneyandcurrency.joda.PersistentMoneyAmount",
      parameters = {@Parameter(name = "currencyCode", value = CurrencyConfig.CURRENCY_CODE)})
  private Money totalCost;

  @Setter
  @Getter
  private Integer numberOfNewPatientsAdded;

  @Setter
  @Getter
  private Integer adjustedConsumption;

  @ElementCollection
  @CollectionTable(
      name = "previous_adjusted_consumptions",
      joinColumns = @JoinColumn(name = "requisitionLineItemId"))
  @Column(name = "previousAdjustedConsumption")
  @Setter
  @Getter
  private List<Integer> previousAdjustedConsumptions;

  @Setter
  @Getter
  private Integer averageConsumption;

  @Getter
  @Setter
  private Integer maximumStockQuantity;

  @Getter
  @Setter
  private Integer calculatedOrderQuantity;

  @OneToMany(
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.LAZY,
      orphanRemoval = true)
  @Getter
  @Setter
  @JoinColumn(name = "requisitionLineItemId")
  private List<StockAdjustment> stockAdjustments;

  @Column
  @Setter
  @Getter
  private BigDecimal maxPeriodsOfStock;

  @Setter
  @Getter
  private boolean nonFullSupply;

  /**
   * Creates new instance of RequisitionLineItem object based on data from
   * {@link RequisitionLineItem.Importer}
   *
   * @param importer instance of {@link Importer}
   * @return new instance of RequisitionLineItem.
   */
  public static RequisitionLineItem newRequisitionLineItem(Importer importer) {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setId(importer.getId());
    if (importer.getOrderable() != null) {
      requisitionLineItem.setOrderableId(importer.getOrderable().getId());
    }
    requisitionLineItem.setBeginningBalance(importer.getBeginningBalance());
    requisitionLineItem.setTotalReceivedQuantity(importer.getTotalReceivedQuantity());
    requisitionLineItem.setTotalLossesAndAdjustments(importer.getTotalLossesAndAdjustments());
    requisitionLineItem.setStockOnHand(importer.getStockOnHand());
    requisitionLineItem.setRequestedQuantity(importer.getRequestedQuantity());
    requisitionLineItem.setTotalConsumedQuantity(importer.getTotalConsumedQuantity());
    requisitionLineItem.setRequestedQuantityExplanation(importer.getRequestedQuantityExplanation());
    requisitionLineItem.setRemarks(importer.getRemarks());
    requisitionLineItem.setApprovedQuantity(importer.getApprovedQuantity());
    requisitionLineItem.setTotalStockoutDays(importer.getTotalStockoutDays());
    requisitionLineItem.setPacksToShip(importer.getPacksToShip());
    requisitionLineItem.setPricePerPack(importer.getPricePerPack());
    requisitionLineItem.setNumberOfNewPatientsAdded(importer.getNumberOfNewPatientsAdded());
    requisitionLineItem.setTotal(importer.getTotal());
    requisitionLineItem.setTotalCost(importer.getTotalCost());
    requisitionLineItem.setAdjustedConsumption(importer.getAdjustedConsumption());
    requisitionLineItem.setAverageConsumption(importer.getAverageConsumption());
    requisitionLineItem.setMaximumStockQuantity(importer.getMaximumStockQuantity());
    requisitionLineItem.setMaxPeriodsOfStock(importer.getMaxPeriodsOfStock());
    requisitionLineItem.setCalculatedOrderQuantity(importer.getCalculatedOrderQuantity());

    List<StockAdjustment> stockAdjustments = new ArrayList<>();
    for (StockAdjustment.Importer stockAdjustmentImporter : importer.getStockAdjustments()) {
      stockAdjustments.add(StockAdjustment.newStockAdjustment(stockAdjustmentImporter));
    }

    requisitionLineItem.setStockAdjustments(stockAdjustments);

    return requisitionLineItem;
  }

  /**
   * Initiates a requisition line item.
   */
  public RequisitionLineItem() {
    stockAdjustments = new ArrayList<>();
    this.skipped = false;
    previousAdjustedConsumptions = new ArrayList<>();
  }

  /**
   * Initiates a requisition line item with specified requisition and product.
   *
   * @param requisition     requisition to apply
   * @param approvedProduct facilityTypeApprovedProduct to apply
   */
  public RequisitionLineItem(Requisition requisition, ApprovedProductDto approvedProduct) {
    this();
    this.requisition = requisition;
    this.maxPeriodsOfStock = BigDecimal.valueOf(approvedProduct.getMaxPeriodsOfStock());
    this.orderableId = approvedProduct.getOrderable().getId();

    ProgramOrderableDto product = approvedProduct.getOrderable()
        .findProgramOrderableDto(approvedProduct.getProgram().getId());

    LOGGER.debug("ProgramOrderableDto {}", product);
    Money priceFromProduct = product.getPricePerPack();
    this.pricePerPack = priceFromProduct == null
        ? Money.of(CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE), PRICE_PER_PACK_IF_NULL)
        : priceFromProduct;
  }

  /**
   * Copy values of attributes into new or updated RequisitionLineItem.
   *
   * @param requisitionLineItem RequisitionLineItem with new values.
   */
  public void updateFrom(RequisitionLineItem requisitionLineItem) {
    if (requisition.isApprovable()) {
      this.approvedQuantity = requisitionLineItem.getApprovedQuantity();
      this.remarks = requisitionLineItem.getRemarks();
    } else {
      this.stockOnHand = requisitionLineItem.getStockOnHand();
      this.beginningBalance = requisitionLineItem.getBeginningBalance();
      this.totalReceivedQuantity = requisitionLineItem.getTotalReceivedQuantity();
      this.totalConsumedQuantity = requisitionLineItem.getTotalConsumedQuantity();
      this.requestedQuantity = requisitionLineItem.getRequestedQuantity();
      this.requestedQuantityExplanation = requisitionLineItem.getRequestedQuantityExplanation();
      this.totalStockoutDays = requisitionLineItem.getTotalStockoutDays();
      this.total = requisitionLineItem.getTotal();
      this.numberOfNewPatientsAdded = requisitionLineItem.getNumberOfNewPatientsAdded();
      this.maximumStockQuantity = requisitionLineItem.getMaximumStockQuantity();
      this.calculatedOrderQuantity = requisitionLineItem.getCalculatedOrderQuantity();
      if (requisitionLineItem.getSkipped() != null) {
        this.skipped = requisitionLineItem.getSkipped();
      } else {
        this.skipped = false;
      }

      if (null == this.stockAdjustments) {
        this.stockAdjustments = new ArrayList<>();
      } else {
        this.stockAdjustments.clear();
      }

      if (null != requisitionLineItem.getStockAdjustments()) {
        stockAdjustments.addAll(requisitionLineItem.getStockAdjustments());
      }

      this.adjustedConsumption = requisitionLineItem.getAdjustedConsumption();
      this.averageConsumption = requisitionLineItem.getAverageConsumption();
    }
  }

  /**
   * Check if all required calculation fields are not filled.
   */
  public boolean allRequiredCalcFieldsNotFilled(String field) {
    switch (field) {
      case TOTAL_CONSUMED_QUANTITY:
        return null == stockOnHand;
      case STOCK_ON_HAND:
        return null == totalConsumedQuantity;
      default:
        return false;
    }
  }

  /**
   * Returns order quantity.
   */
  public Integer getOrderQuantity() {
    if (!requisition.getStatus().isPreAuthorize() && approvedQuantity != null) {
      return approvedQuantity;
    }

    if (requestedQuantity != null) {
      return requestedQuantity;
    }

    if (calculatedOrderQuantity != null) {
      return calculatedOrderQuantity;
    }

    return 0;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Exporter exporter, OrderableDto orderableDto) {
    exporter.setId(id);
    exporter.setBeginningBalance(beginningBalance);
    exporter.setTotalReceivedQuantity(totalReceivedQuantity);
    exporter.setTotalLossesAndAdjustments(totalLossesAndAdjustments);
    exporter.setStockOnHand(stockOnHand);
    exporter.setRequestedQuantity(requestedQuantity);
    exporter.setTotalConsumedQuantity(totalConsumedQuantity);
    exporter.setRequestedQuantityExplanation(requestedQuantityExplanation);
    exporter.setRemarks(remarks);
    exporter.setApprovedQuantity(approvedQuantity);
    exporter.setStockAdjustments(stockAdjustments);
    exporter.setTotalStockoutDays(totalStockoutDays);
    exporter.setTotal(total);
    exporter.setPacksToShip(packsToShip);
    exporter.setOrderable(orderableDto);
    exporter.setPricePerPack(pricePerPack);
    exporter.setNumberOfNewPatientsAdded(numberOfNewPatientsAdded);
    exporter.setTotalCost(totalCost);
    exporter.setSkipped(skipped);
    exporter.setAdjustedConsumption(adjustedConsumption);
    exporter.setPreviousAdjustedConsumptions(previousAdjustedConsumptions);
    exporter.setMaximumStockQuantity(maximumStockQuantity);
    exporter.setMaxPeriodsOfStock(maxPeriodsOfStock);
    exporter.setAverageConsumption(averageConsumption);
    exporter.setCalculatedOrderQuantity(calculatedOrderQuantity);
  }

  /**
   * Resets all quantities and adjustments of a line item.
   */
  public void resetData() {
    setBeginningBalance(null);
    setTotalReceivedQuantity(null);
    setTotalLossesAndAdjustments(null);
    setStockOnHand(null);
    setRequestedQuantityExplanation(null);
    setRemarks(null);
    setApprovedQuantity(null);
    setRequestedQuantity(null);
    setTotalConsumedQuantity(null);
    setTotal(null);
    setRequestedQuantityExplanation(null);
    setTotalStockoutDays(null);
    setPacksToShip(null);
    setTotalCost(null);
    setNumberOfNewPatientsAdded(null);
    setAdjustedConsumption(null);
    setAverageConsumption(null);
    setMaximumStockQuantity(null);
    setCalculatedOrderQuantity(null);
    stockAdjustments.clear();
    previousAdjustedConsumptions.clear();
  }

  /**
   * Skip requisition line item of column is displayed on template.
   *
   * @param template on which we check if column is displayed
   */
  public void skipLineItem(RequisitionTemplate template) {
    if (template.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN)) {
      skipped = true;
    } else {
      throw new ValidationMessageException(new Message("requisition.error.can-not-skip"));
    }
  }

  /**
   * Calculate and set all calculated fields in this requisition line item.
   */
  public void calculateAndSetFields(RequisitionTemplate template,
                                    Collection<StockAdjustmentReason> stockAdjustmentReasons,
                                    Integer numberOfMonthsInPeriod) {
    calculateAndSetTotalLossesAndAdjustments(stockAdjustmentReasons);
    calculateAndSetStockOnHand(template);
    calculateAndSetTotalConsumedQuantity(template);
    calculateAndSetTotal(template);
    calculateAndSetAdjustedConsumption(template, numberOfMonthsInPeriod);
    calculateAndSetAverageConsumption(template);
    calculateAndSetMaximumStockQuantity(template);
    calculateAndSetCalculatedOrderQuantity(template);
  }

  /**
   * Recalculates packs to ship.
   *
   * @param products list of orderable products.
   */
  public void updatePacksToShip(Collection<OrderableDto> products) {
    this.packsToShip = products.stream()
        .filter(product -> product.getId().equals(getOrderableId()))
        .map(product -> getOrderQuantity() != null
            ? product.packsToOrder(getOrderQuantity()) : null)
        .findFirst()
        .orElse(null);
  }

  /**
   * Sets appropriate value for Average Consumption field in {@link RequisitionLineItem}.
   */
  void calculateAndSetAverageConsumption() {
    List<Integer> previous = new ArrayList<>(getPreviousAdjustedConsumptions());
    previous.add(getAdjustedConsumption());
    Integer calculated = calculateAverageConsumption(previous);
    setAverageConsumption(calculated);
  }

  /**
   * Sets appropriate value for Adjusted Consumption field in {@link RequisitionLineItem} on update.
   */
  private void calculateAndSetAverageConsumption(RequisitionTemplate template) {
    if (template.isColumnInTemplate(AVERAGE_CONSUMPTION)) {
      Integer averageConsumptionPassed = this.getAverageConsumption();
      calculateAndSetAverageConsumption();

      if (averageConsumptionPassed != null
          && !Objects.equals(averageConsumptionPassed, getAverageConsumption())) {
        LOGGER.warn("Passed Average Consumption does not match calculated one.");
      }
    }
  }

  /**
   * Sets appropriate value for Total Consumed Quantity field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetTotalConsumedQuantity(RequisitionTemplate template) {
    if (template.isColumnDisplayed(TOTAL_CONSUMED_QUANTITY)) {
      if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)) {
        int calculated = calculateTotalConsumedQuantity(this);
        if (getTotalConsumedQuantity() != null
            && !Objects.equals(getTotalConsumedQuantity(), calculated)) {
          LOGGER.warn("Passed TotalConsumedQuantity does not match calculated one.");
        }
        setTotalConsumedQuantity(calculated);
      }
    } else {
      setTotalConsumedQuantity(null);
    }
  }

  /**
   * Sets appropriate value for Total field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetTotal(RequisitionTemplate template) {
    if (template.isColumnDisplayed(TOTAL_COLUMN)) {
      int calculated = calculateTotal(this);
      if (getTotal() != null
          && !Objects.equals(getTotal(), calculated)) {
        LOGGER.warn("Passed Total does not match calculated one.");
      }
      setTotal(calculated);
    }
  }

  /**
   * Sets appropriate value for Stock On Hand field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetStockOnHand(RequisitionTemplate template) {
    if (template.isColumnDisplayed(STOCK_ON_HAND)) {
      if (template.isColumnCalculated(STOCK_ON_HAND)) {
        int calculated = calculateStockOnHand(this);
        if (getStockOnHand() != null
            && !Objects.equals(getStockOnHand(), calculated)) {
          LOGGER.warn("Passed StockOnHand does not match calculated one.");
        }
        setStockOnHand(calculated);
      }
    } else {
      setStockOnHand(null);
    }
  }

  /**
   * Sets appropriate value for Total Consumed Quantity field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetTotalLossesAndAdjustments(
      Collection<StockAdjustmentReason> reasons) {
    int calculated = calculateTotalLossesAndAdjustments(this, reasons);
    if (getTotalLossesAndAdjustments() != null
        && !Objects.equals(getTotalLossesAndAdjustments(), calculated)) {
      LOGGER.warn("Passed TotalLossesAndAdjustments does not match calculated one.");
    }
    setTotalLossesAndAdjustments(calculated);
  }

  /**
   * Sets appropriate value for Adjusted Consumption field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetAdjustedConsumption(RequisitionTemplate template,
                                                 Integer monthsInThePeriod) {
    if (template.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
      int calculated = calculateAdjustedConsumption(this, monthsInThePeriod);

      if (getAdjustedConsumption() != null
          && !Objects.equals(getAdjustedConsumption(), calculated)) {
        LOGGER.warn("Passed Adjusted Consumption does not match calculated one.");
      }
      setAdjustedConsumption(calculated);
    }
  }

  /**
   * Sets appropriate value for Maximum Stock Quantity field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetMaximumStockQuantity(RequisitionTemplate template) {
    if (template.isColumnDisplayed(MAXIMUM_STOCK_QUANTITY)) {
      int calculated = calculateMaximumStockQuantity(this, template);
      if (getMaximumStockQuantity() != null
          && !Objects.equals(getMaximumStockQuantity(), calculated)) {
        LOGGER.warn("Passed MaximumStockQuantity does not match calculated one.");
      }
      setMaximumStockQuantity(calculated);
    }
  }

  /**
   * Sets appropriate value for Calculated Order Quantity field in {@link RequisitionLineItem}.
   */
  private void calculateAndSetCalculatedOrderQuantity(RequisitionTemplate template) {
    if (template.isColumnDisplayed(CALCULATED_ORDER_QUANTITY)) {
      int calculated = calculateCalculatedOrderQuantity(this, template);
      if (getCalculatedOrderQuantity() != null
          && !Objects.equals(getCalculatedOrderQuantity(), calculated)) {
        LOGGER.warn("Passed CalculatedOrderQuantity does not match calculated one.");
      }
      setCalculatedOrderQuantity(calculated);
    }
  }

  public interface Exporter {
    void setId(UUID id);

    void setBeginningBalance(Integer beginningBalance);

    void setTotalReceivedQuantity(Integer totalReceivedQuantity);

    void setTotalLossesAndAdjustments(Integer totalLossesAndAdjustments);

    void setStockOnHand(Integer stockOnHand);

    void setRequestedQuantity(Integer requestedQuantity);

    void setTotalConsumedQuantity(Integer totalConsumedQuantity);

    void setRequestedQuantityExplanation(String requestedQuantityExplanation);

    void setRemarks(String remarks);

    void setApprovedQuantity(Integer approvedQuantity);

    void setStockAdjustments(List<StockAdjustment> stockAdjustments);

    void setTotalStockoutDays(Integer totalStockoutDays);

    void setTotal(Integer total);

    void setPacksToShip(Long packsToShip);

    void setOrderable(OrderableDto orderableDto);

    void setPricePerPack(Money pricePerPack);

    void setNumberOfNewPatientsAdded(Integer numberOfNewPatientsAdded);

    void setTotalCost(Money totalCost);

    void setSkipped(Boolean skipped);

    void setAdjustedConsumption(Integer adjustedConsumption);

    void setPreviousAdjustedConsumptions(List<Integer> previousAdjustedConsupmtions);

    void setAverageConsumption(Integer averageConsumption);

    void setMaxPeriodsOfStock(BigDecimal maxPeriodsOfStock);

    void setMaximumStockQuantity(Integer maximumStockQuantity);

    void setCalculatedOrderQuantity(Integer calculatedOrderQuantity);
  }

  public interface Importer {
    UUID getId();

    Integer getBeginningBalance();

    Integer getTotalReceivedQuantity();

    OrderableDto getOrderable();

    Integer getTotalLossesAndAdjustments();

    Integer getStockOnHand();

    Integer getRequestedQuantity();

    Integer getTotalConsumedQuantity();

    String getRequestedQuantityExplanation();

    String getRemarks();

    Integer getApprovedQuantity();

    List<StockAdjustment.Importer> getStockAdjustments();

    Integer getTotalStockoutDays();

    Integer getTotal();

    Long getPacksToShip();

    Money getPricePerPack();

    Integer getNumberOfNewPatientsAdded();

    Money getTotalCost();

    Boolean getSkipped();

    Integer getAdjustedConsumption();

    Integer getAverageConsumption();

    BigDecimal getMaxPeriodsOfStock();

    Integer getMaximumStockQuantity();

    Integer getCalculatedOrderQuantity();
  }
}
