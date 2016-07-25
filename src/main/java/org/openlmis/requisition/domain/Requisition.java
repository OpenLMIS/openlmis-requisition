package org.openlmis.requisition.domain;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;

import java.time.LocalDateTime;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

@Entity
@Table(name = "requisitions")
@NoArgsConstructor
public class Requisition extends BaseEntity {

  @JsonSerialize(using = LocalDateTimeSerializer.class)
  @JsonDeserialize(using = LocalDateTimeDeserializer.class)
  @Getter
  @Setter
  private LocalDateTime createdDate;

  @OneToMany(mappedBy = "requisition", cascade = {CascadeType.REMOVE}, fetch = FetchType.EAGER)
  @Getter
  @Setter
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

  @ManyToOne
  @JoinColumn(name = "processingPeriodId", nullable = false)
  @Getter
  @Setter
  private Period processingPeriod;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  @Getter
  @Setter
  private RequisitionStatus status;

  @ManyToOne
  @JoinColumn(name = "creatorId")
  @Getter
  @Setter
  private User creator;

  @Column
  @Getter
  @Setter
  private Boolean emergency;

  @ManyToOne
  @JoinColumn(name = "supervisoryNodeId")
  @Getter
  @Setter
  private SupervisoryNode supervisoryNode;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
  }
}
