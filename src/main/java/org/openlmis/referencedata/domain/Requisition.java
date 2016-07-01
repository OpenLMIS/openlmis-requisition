package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

@Entity
@Table(name = "requisition")
@NoArgsConstructor
public class Requisition extends BaseEntity {

  @Getter
  @Setter
  private LocalDateTime createdDate;

  @OneToMany(mappedBy = "requisition")
  @Getter
  private Set<RequisitionLine> requisitionLines;

  @OneToOne
  @JoinColumn(name = "facilityId", nullable = false)
  @Getter
  @Setter
  private Facility facility;

  @OneToOne
  @JoinColumn(name = "programId", nullable = false)
  @Getter
  @Setter
  private Program program;

  @OneToOne
  @JoinColumn(name = "processingPeriodId", nullable = false)
  @Getter
  @Setter
  private Period processingPeriod;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  @Getter
  @Setter
  private RequisitionStatus status;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
    this.status = RequisitionStatus.INITIATED;
  }
}
