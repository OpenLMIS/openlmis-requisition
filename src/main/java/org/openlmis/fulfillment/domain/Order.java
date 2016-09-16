package org.openlmis.fulfillment.domain;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.fulfillment.utils.LocalDateTimePersistenceConverter;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "orders")
@NoArgsConstructor
@JsonIdentityInfo(generator = ObjectIdGenerators.PropertyGenerator.class, property = "id")
public class Order extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "requisitionId")
  @Getter
  @Setter
  private Requisition requisition;

  @JsonSerialize(using = LocalDateTimeSerializer.class)
  @JsonDeserialize(using = LocalDateTimeDeserializer.class)
  @Convert(converter = LocalDateTimePersistenceConverter.class)
  @Getter
  @Setter
  private LocalDateTime createdDate;

  @Setter
  private UUID createdById;

  @Setter
  private UUID program;

  @Setter
  private UUID requestingFacility;

  @Setter
  private UUID receivingFacility;

  @Setter
  private UUID supplyingFacility;

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

  // TODO: determine why it has to be set explicitly
  @JsonIdentityInfo(
      generator = ObjectIdGenerators.IntSequenceGenerator.class,
      property = "orderLinesId")
  @OneToMany(
      mappedBy = "order",
      cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER,
      orphanRemoval = true)
  @Getter
  @Setter
  private List<OrderLine> orderLines;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
  }

  /**
   * Copy values of attributes into new or updated Order.
   *
   * @param order Order with new values.
   */
  public void updateFrom(Order order) {
    this.requisition = order.requisition;
    this.createdById = order.createdById;
    this.program = order.program;
    this.requestingFacility = order.requestingFacility;
    this.receivingFacility = order.receivingFacility;
    this.supplyingFacility = order.supplyingFacility;
    this.orderCode = order.orderCode;
    this.status = order.status;
    this.quotedCost = order.quotedCost;
  }

  public UserDto getCreatedById() {
    //TODO in Sprint 9
    return null;
  }

  public ProgramDto getProgram() {
    //TODO in Sprint 9
    return null;
  }

  public FacilityDto getRequestingFacility() {
    //TODO in Sprint 9
    return null;
  }

  public FacilityDto getReceivingFacility() {
    //TODO in Sprint 9
    return null;
  }

  public FacilityDto getSupplyingFacility() {
    //TODO in Sprint 9
    return null;
  }
}
