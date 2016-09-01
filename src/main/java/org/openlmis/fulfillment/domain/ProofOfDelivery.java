package org.openlmis.fulfillment.domain;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import lombok.Getter;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;
import java.util.List;


@Entity
@Table(name = "proof_of_deliveries")
public class ProofOfDelivery extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "orderId", nullable = false)
  @Getter
  @Setter
  private Order order;

  @JsonIdentityInfo(
        generator = ObjectIdGenerators.IntSequenceGenerator.class,
        property = "proofOfDeliveryLineItemsId")
  @OneToMany(mappedBy = "proofOfDelivery",
      cascade = {CascadeType.MERGE, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER)
  @Getter
  @Setter
  private List<ProofOfDeliveryLine> proofOfDeliveryLineItems;

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
    this.proofOfDeliveryLineItems = proofOfDelivery.getProofOfDeliveryLineItems();
    this.totalShippedPacks = proofOfDelivery.getTotalShippedPacks();
    this.totalReceivedPacks = proofOfDelivery.getTotalReceivedPacks();
    this.totalReturnedPacks = proofOfDelivery.getTotalReturnedPacks();
    this.deliveredBy = proofOfDelivery.getDeliveredBy();
    this.receivedBy = proofOfDelivery.getReceivedBy();
    this.receivedDate = proofOfDelivery.getReceivedDate();
  }
}
