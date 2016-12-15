package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.Type;
import org.openlmis.requisition.dto.FacilityTypeApprovedProductDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProgramProductDto;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "requisition_line_items")
public class RequisitionLineItem extends BaseEntity {

  public static final String REQUESTED_QUANTITY = "requestedQuantity";
  public static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  public static final String BEGINNING_BALANCE = "beginningBalance";
  public static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  public static final String STOCK_ON_HAND = "stockOnHand";
  public static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  public static final String TOTAL_LOSSES_AND_ADJUSTMENTS = "totalLossesAndAdjustments";
  public static final String APPROVED_QUANTITY = "approvedQuantity";
  public static final String REMARKS = "remarks";
  public static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";
  public static final String TOTAL = "total";

  static final BigDecimal PRICE_PER_PACK_IF_NULL = BigDecimal.ZERO;

  private static final String UUID = "pg-uuid";

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID orderableProductId;

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "requisitionId")
  @Getter
  @Setter
  private Requisition requisition;

  @Column
  @Getter
  @Setter
  private Integer beginningBalance;

  @Column
  @Getter
  @Setter
  private Integer totalReceivedQuantity;

  @Column
  @Getter
  @Setter
  private Integer totalLossesAndAdjustments;

  @Column
  @Getter
  @Setter
  private Integer stockOnHand;

  @Column
  @Getter
  @Setter
  private Integer requestedQuantity;

  @Column
  @Getter
  @Setter
  private Integer totalConsumedQuantity;

  @Column
  @Getter
  @Setter
  private Integer total;

  @Column
  @Getter
  @Setter
  private String requestedQuantityExplanation;

  @Column(length = 250)
  @Getter
  @Setter
  private String remarks;

  @Column
  @Getter
  @Setter
  private Integer approvedQuantity;

  @Column
  @Getter
  @Setter
  private Integer totalStockoutDays;

  @Column
  @Getter
  @Setter
  private Long packsToShip;

  @Getter
  @Setter
  @Embedded
  private Money pricePerPack;

  @OneToMany(
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER,
      orphanRemoval = true)
  @Getter
  @Setter
  @JoinColumn(name = "requisitionLineItemId")
  private List<StockAdjustment> stockAdjustments;

  @Transient
  @Setter
  @Getter(AccessLevel.PACKAGE)
  private boolean nonFullSupply;

  public RequisitionLineItem() {
    stockAdjustments = new ArrayList<>();
  }

  /**
   * Initiates a requisition line item with specified requisition and product.
   *
   * @param requisition                 requisition to apply
   * @param facilityTypeApprovedProduct facilityTypeApprovedProduct to apply
   */
  public RequisitionLineItem(
      Requisition requisition, FacilityTypeApprovedProductDto facilityTypeApprovedProduct) {
    this();
    this.requisition = requisition;

    ProgramProductDto programProduct = facilityTypeApprovedProduct.getProgramProduct();
    this.orderableProductId = programProduct.getProductId();

    Money priceFromProduct = programProduct.getPricePerPack();
    this.pricePerPack = priceFromProduct == null
            ? new Money(PRICE_PER_PACK_IF_NULL) : priceFromProduct;
  }

  /**
   * Copy values of attributes into new or updated RequisitionLineItem.
   *
   * @param requisitionLineItem RequisitionLineItem with new values.
   */
  public void updateFrom(RequisitionLineItem requisitionLineItem) {
    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
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

      if (null == this.stockAdjustments) {
        this.stockAdjustments = new ArrayList<>();
      } else {
        this.stockAdjustments.clear();
      }

      if (null != requisitionLineItem.getStockAdjustments()) {
        stockAdjustments.addAll(requisitionLineItem.getStockAdjustments());
      }
    }
  }

  boolean allRequiredCalcFieldsNotFilled(String field) {
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
  @JsonIgnore
  public Integer getOrderQuantity() {
    if (approvedQuantity != null) {
      return approvedQuantity;
    }

    if (requestedQuantity != null) {
      return requestedQuantity;
    }

    return 0;
  }

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
    if (importer.getOrderableProduct() != null) {
      requisitionLineItem.setOrderableProductId(importer.getOrderableProduct().getId());
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
    requisitionLineItem.setTotal(importer.getTotal());
    requisitionLineItem.setPacksToShip(importer.getPacksToShip());
    requisitionLineItem.setPricePerPack(importer.getPricePerPack());

    List<StockAdjustment> stockAdjustments = new ArrayList<>();
    if (importer.getStockAdjustments() != null) {
      for (StockAdjustment.Importer stockAdjustmentImporter : importer.getStockAdjustments()) {
        stockAdjustments.add(StockAdjustment.newStockAdjustment(stockAdjustmentImporter));
      }
    }

    requisitionLineItem.setStockAdjustments(stockAdjustments);

    return requisitionLineItem;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Exporter exporter, OrderableProductDto orderableProductDto) {
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
    exporter.setOrderableProduct(orderableProductDto);
    exporter.setPricePerPack(pricePerPack);
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

    void setOrderableProduct(OrderableProductDto orderableProductDto);

    void setPricePerPack(Money pricePerPack);
  }

  public interface Importer {
    UUID getId();

    Integer getBeginningBalance();

    Integer getTotalReceivedQuantity();

    OrderableProductDto getOrderableProduct();

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
  }
}
