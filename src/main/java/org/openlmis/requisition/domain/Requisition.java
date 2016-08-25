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
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "requisitions")
@NoArgsConstructor
public class Requisition extends BaseEntity {

  @JsonSerialize(using = LocalDateTimeSerializer.class)
  @JsonDeserialize(using = LocalDateTimeDeserializer.class)
  @Getter
  @Setter
  private LocalDateTime createdDate;

  // TODO: determine why it has to be set explicitly
  @OneToMany(mappedBy = "requisition",
      cascade = {CascadeType.MERGE, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER)
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

  @ManyToOne
  @JoinColumn(name = "facilityId", nullable = false)
  @Getter
  @Setter
  private Facility facility;

  @ManyToOne
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

  @Column
  @Getter
  @Setter
  private Boolean emergency;

  @Column(length = 250)
  @Getter
  @Setter
  private String remarks;

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
