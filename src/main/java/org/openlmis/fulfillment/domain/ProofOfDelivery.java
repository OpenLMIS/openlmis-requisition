package org.openlmis.fulfillment.domain;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import lombok.Getter;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.util.Date;
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
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE})
  @Getter
  private List<ProofOfDeliveryLine> proofOfDeliveryLineItems;

  @Column
  @Getter
  @Setter
  private Integer totalShippedPacks;

  @Column
  @Getter
  @Setter
  private Integer totalReceivedPacks;

  @Column
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

  @Column
  @Getter
  @Setter
  private Date receivedDate;
}
