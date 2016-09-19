package org.openlmis.requisition.domain;

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
import org.openlmis.requisition.exception.RequisitionException;

import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import java.util.UUID;

@Entity
@Table(name = "requisitions")
@NoArgsConstructor
public class Requisition extends BaseEntity {

  @JsonSerialize(using = LocalDateTimeSerializer.class)
  @JsonDeserialize(using = LocalDateTimeDeserializer.class)
  @Convert(converter = LocalDateTimePersistenceConverter.class)
  @Getter
  @Setter
  private LocalDateTime createdDate;

  // TODO: determine why it has to be set explicitly
  @OneToMany(
      mappedBy = "requisition",
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER,
      orphanRemoval = true)
  @Getter
  @Setter
  @JsonIdentityInfo(
      generator = ObjectIdGenerators.IntSequenceGenerator.class,
      property = "requisitionLinesId")
  private List<RequisitionLine> requisitionLines;

  @JsonIdentityInfo(
      generator = ObjectIdGenerators.IntSequenceGenerator.class,
      property = "commentsId")
  @OneToMany(mappedBy = "requisition", cascade = CascadeType.REMOVE)
  @Getter
  private List<Comment> comments;

  @Getter
  @Setter
  private UUID facility;

  @Getter
  @Setter
  private UUID program;

  @Getter
  @Setter
  private UUID processingPeriod;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  @Getter
  @Setter
  private RequisitionStatus status;

  @Column
  @Getter
  @Setter
  private Boolean emergency;

  @Getter
  @Setter
  private UUID supervisoryNode;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
  }

  /**
   * Copy values of attributes into new or updated Requisition.
   *
   * @param requisition Requisition with new values.
   */
  public void updateFrom(Requisition requisition) {
    this.comments = requisition.getComments();
    this.facility = requisition.getFacility();
    this.program = requisition.getProgram();
    this.processingPeriod = requisition.getProcessingPeriod();
    this.emergency = requisition.getEmergency();
    this.supervisoryNode = requisition.getSupervisoryNode();
  }

  /**
   * Submits given requisition.
   */
  public void submit() throws RequisitionException {
    if (!RequisitionStatus.INITIATED.equals(status)) {
      throw new RequisitionException("Cannot submit requisition: " + getId()
          + ", requisition must have status 'INITIATED' to be submitted.");
    }

    status = RequisitionStatus.SUBMITTED;
    calculateStockOnHand();
  }

  /**
   * Authorize given Requisition.
   */
  public void authorize() throws RequisitionException {
    if (!RequisitionStatus.SUBMITTED.equals(status)) {
      throw new RequisitionException("Cannot authorize requisition: " + getId()
          + ", requisition must have status 'SUBMITTED' to be authorized.");
    }

    status = RequisitionStatus.AUTHORIZED;
    calculateStockOnHand();
  }

  private void calculateStockOnHand() {
    if (requisitionLines != null) {
      for (RequisitionLine requisitionLine : requisitionLines) {
        requisitionLine.calculateStockOnHand();
      }
    }
  }
}
