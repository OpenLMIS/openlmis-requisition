package org.openlmis.fulfillment.domain;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.utils.LocalDatePersistenceConverter;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;


@Entity
@Table(name = "proof_of_deliveries")
@JsonIdentityInfo(generator = ObjectIdGenerators.PropertyGenerator.class, property = "id")
public class ProofOfDelivery extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "orderId", nullable = false)
  @Getter
  @Setter
  private Order order;

  @OneToMany(
      mappedBy = "proofOfDelivery",
      cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER,
      orphanRemoval = true)
  @Fetch(FetchMode.SELECT)
  @Getter
  @Setter
  private List<ProofOfDeliveryLineItem> proofOfDeliveryLineItems;

  @Getter
  @Setter
  private Integer totalShippedPacks;

  @Getter
  @Setter
  private Integer totalReceivedPacks;

  @Getter
  @Setter
  private Integer totalReturnedPacks;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String deliveredBy;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String receivedBy;

  @JsonSerialize(using = LocalDateSerializer.class)
  @JsonDeserialize(using = LocalDateDeserializer.class)
  @Convert(converter = LocalDatePersistenceConverter.class)
  @Getter
  @Setter
  private LocalDate receivedDate;

  /**
   * Copy values of attributes into new or updated ProofOfDelivery.
   *
   * @param proofOfDelivery ProofOfDelivery with new values.
   */
  public void updateFrom(ProofOfDelivery proofOfDelivery) {
    this.order = proofOfDelivery.order;
    this.totalShippedPacks = proofOfDelivery.getTotalShippedPacks();
    this.totalReceivedPacks = proofOfDelivery.getTotalReceivedPacks();
    this.totalReturnedPacks = proofOfDelivery.getTotalReturnedPacks();
    this.deliveredBy = proofOfDelivery.getDeliveredBy();
    this.receivedBy = proofOfDelivery.getReceivedBy();
    this.receivedDate = proofOfDelivery.getReceivedDate();
  }
}
