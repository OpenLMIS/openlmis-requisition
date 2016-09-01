package org.openlmis.hierarchyandsupervision.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "supply_lines", schema = "referencedata")
@NoArgsConstructor
public class SupplyLine extends BaseEntity {

  @ManyToOne
  @JoinColumn(name = "supervisoryNodeId", nullable = false)
  @Getter
  @Setter
  private SupervisoryNode supervisoryNode;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String description;

  @ManyToOne
  @JoinColumn(name = "programId", nullable = false)
  @Getter
  @Setter
  private Program program;

  @ManyToOne
  @JoinColumn(name = "supplyingFacilityId", nullable = false)
  @Getter
  @Setter
  private Facility supplyingFacility;

  /**
   * Copy values of attributes into new or updated SupplyLine.
   *
   * @param supplyLine SupplyLine with new values.
   */
  public void updateFrom(SupplyLine supplyLine) {
    this.supervisoryNode = supplyLine.getSupervisoryNode();
    this.description = supplyLine.getDescription();
    this.program = supplyLine.getProgram();
    this.supplyingFacility = supplyLine.getSupplyingFacility();
  }
}
