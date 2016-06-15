package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;

@Entity
@Table(name = "orders")
@NoArgsConstructor
public class Order extends BaseEntity {

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String requisitionCode;

  @Getter
  @Setter
  private LocalDateTime createdDate;

  @ManyToOne
  @JoinColumn(name = "userId", nullable = false)
  @Getter
  @Setter
  private User createdBy;

  @ManyToOne
  @JoinColumn(name = "programId", nullable = false)
  @Getter
  @Setter
  private Program program;

  @ManyToOne
  @JoinColumn(name = "requestingFacilityId", nullable = false)
  @Getter
  @Setter
  private Facility requestingFacility;

  @ManyToOne
  @JoinColumn(name = "receivingFacilityId", nullable = false)
  @Getter
  @Setter
  private Facility receivingFacility;

  @ManyToOne
  @JoinColumn(name = "supplyingFacilityId", nullable = false)
  @Getter
  @Setter
  private Facility supplyingFacility;

  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String orderCode;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  @Getter
  @Setter
  private OrderStatus status;

  @Column(nullable = false)
  @Getter
  @Setter
  private BigDecimal quotedCost;

  @OneToMany(mappedBy = "order")
  @Getter
  private Set<OrderLine> orderLines;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
  }
}
