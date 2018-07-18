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

package org.openlmis.requisition.domain.requisition;

import static org.openlmis.requisition.CurrencyConfig.CURRENCY_CODE;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.assertj.core.util.Lists;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineItemDataBuilder {
  private UUID id = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();
  private Requisition requisition = new Requisition();
  private Integer beginningBalance = 100;
  private Integer totalReceivedQuantity = 50;
  private Integer totalLossesAndAdjustments = 0;
  private Integer stockOnHand = 50;
  private Integer requestedQuantity = 100;
  private Integer totalConsumedQuantity = 100;
  private Integer total = 150;
  private String requestedQuantityExplanation = "we need more";
  private String remarks;
  private Integer approvedQuantity;
  private Integer totalStockoutDays = 0;
  private Long packsToShip = 5L;
  private Boolean skipped = false;
  private Money pricePerPack = asMoney(12);
  private Money totalCost = asMoney(60);
  private Integer numberOfNewPatientsAdded = 0;
  private Integer adjustedConsumption = 100;
  private List<Integer> previousAdjustedConsumptions = Lists.emptyList();
  private Integer averageConsumption = 100;
  //this needs to be always averageConsumption * MaxPeriodsOfStock
  private Integer maximumStockQuantity = 300;
  //this needs to be always maximumStockQuantity - stockOnHand
  private Integer calculatedOrderQuantity = 250;
  private List<StockAdjustment> stockAdjustments = new ArrayList<>();
  private BigDecimal maxPeriodsOfStock = BigDecimal.valueOf(3);
  private boolean nonFullSupply = false;
  private Integer idealStockAmount = 100;
  //this needs to be always idealStockAmount - stockOnHand
  private Integer calculatedOrderQuantityIsa = 50;
  private Integer additionalQuantityRequired = 0;

  public RequisitionLineItemDataBuilder withId(UUID id) {
    this.id = id;
    return this;
  }

  public RequisitionLineItemDataBuilder withNonFullSupplyFlag() {
    this.nonFullSupply = true;
    return this;
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem build() {
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildSkipped() {
    this.skipped = true;
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildWithIncorrectStockOnHand() {
    stockOnHand = 10;
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildWithIncorrectMaximumStockQuantity() {
    maximumStockQuantity = 1000;
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildWithIncorrectCalculatedOrderQuantityIsa() {
    calculatedOrderQuantityIsa = 1;
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildWithApprovedQuantity() {
    approvedQuantity = 100;
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildWithRemarks() {
    remarks = "OK";
    return buildForInitiatedRegularRequisition();
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public RequisitionLineItem buildForInitiatedRegularRequisition() {
    RequisitionLineItem lineItem = new RequisitionLineItem(
        orderableId, requisition, beginningBalance, totalReceivedQuantity,
        totalLossesAndAdjustments, stockOnHand, requestedQuantity, totalConsumedQuantity, total,
        requestedQuantityExplanation, remarks, approvedQuantity, totalStockoutDays, packsToShip,
        skipped, pricePerPack, totalCost, numberOfNewPatientsAdded, additionalQuantityRequired,
        adjustedConsumption, previousAdjustedConsumptions, averageConsumption, maximumStockQuantity,
        calculatedOrderQuantity, stockAdjustments, maxPeriodsOfStock, nonFullSupply,
        idealStockAmount, calculatedOrderQuantityIsa
    );
    lineItem.setId(id);

    return lineItem;
  }

  public RequisitionLineItemDataBuilder setTotalStockoutDays(Integer totalStockoutDays) {
    this.totalStockoutDays = totalStockoutDays;
    return this;
  }

  public RequisitionLineItemDataBuilder setApprovedQuantity(Integer approvedQuantity) {
    this.approvedQuantity = approvedQuantity;
    return this;
  }

  public RequisitionLineItemDataBuilder setStockOnHand(Integer stockOnHand) {
    this.stockOnHand = stockOnHand;
    return this;
  }

  public RequisitionLineItemDataBuilder setTotalConsumedQuantity(Integer totalConsumedQuantity) {
    this.totalConsumedQuantity = totalConsumedQuantity;
    return this;
  }

  public RequisitionLineItemDataBuilder setTotalReceivedQuantity(Integer totalReceivedQuantity) {
    this.totalReceivedQuantity = totalReceivedQuantity;
    return this;
  }

  public RequisitionLineItemDataBuilder setBeginningBalance(Integer beginningBalance) {
    this.beginningBalance = beginningBalance;
    return this;
  }

  public RequisitionLineItemDataBuilder setTotal(Integer total) {
    this.total = total;
    return this;
  }

  public RequisitionLineItemDataBuilder addStockAdjustment(StockAdjustment adjustment) {
    this.stockAdjustments.add(adjustment);
    return this;
  }

  private Money asMoney(Number value) {
    return Money.of(CurrencyUnit.of(CURRENCY_CODE), value.doubleValue());
  }

  public RequisitionLineItemDataBuilder setRequisition(Requisition requisition) {
    this.requisition = requisition;
    return this;
  }

  /**
   * Sets approved product.
   */
  public RequisitionLineItemDataBuilder setApprovedProduct(ApprovedProductDto approvedProduct) {
    this.maxPeriodsOfStock = BigDecimal.valueOf(approvedProduct.getMaxPeriodsOfStock());

    OrderableDto orderable = approvedProduct.getOrderable();
    ProgramOrderableDto programOrderable = orderable
        .findProgramOrderableDto(requisition.getProgramId());

    this.orderableId = orderable.getId();

    if (null != programOrderable) {
      this.pricePerPack = Optional
          .ofNullable(programOrderable.getPricePerPack())
          .orElseGet(() -> Money.of(CurrencyUnit.of(CURRENCY_CODE), BigDecimal.ZERO));
    }

    return this;
  }

  public RequisitionLineItemDataBuilder setIdealStockAmount(int idealStockAmount) {
    this.idealStockAmount = idealStockAmount;
    return this;
  }

  public RequisitionLineItemDataBuilder setTotalLossesAndAdjustments(
      int totalLossesAndAdjustments) {
    this.totalLossesAndAdjustments = totalLossesAndAdjustments;
    return this;
  }

  public RequisitionLineItemDataBuilder setStockAdjustments(
      List<StockAdjustment> stockAdjustments) {
    this.stockAdjustments = stockAdjustments;
    return this;
  }

  public RequisitionLineItemDataBuilder setRequestedQuantity(int requestedQuantity) {
    this.requestedQuantity = requestedQuantity;
    return this;
  }

  public RequisitionLineItemDataBuilder setRemarks(String remarks) {
    this.remarks = remarks;
    return this;
  }

  public RequisitionLineItemDataBuilder setRequestedQuantityExplanation(
      String requestedQuantityExplanation) {
    this.requestedQuantityExplanation = requestedQuantityExplanation;
    return this;
  }

  public RequisitionLineItemDataBuilder setTotalCost(Money totalCost) {
    this.totalCost = totalCost;
    return this;
  }

  public RequisitionLineItemDataBuilder setNumberOfNewPatientsAdded(int numberOfNewPatientsAdded) {
    this.numberOfNewPatientsAdded = numberOfNewPatientsAdded;
    return this;
  }
}
