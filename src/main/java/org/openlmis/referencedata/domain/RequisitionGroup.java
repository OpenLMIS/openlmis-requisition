package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 * RequisitionGroup represents a group of facilities which follow a particular schedule for
 * a program. It also defines the contract for creation/upload of RequisitionGroup.
 */
@Entity
@Table(name = "requisition_groups", schema = "referencedata")
@NoArgsConstructor
public class RequisitionGroup extends BaseEntity {

  @Column(unique = true, nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String code;

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String description;

  @OneToOne
  @JoinColumn(name = "supervisoryNodeId", nullable = false)
  @Getter
  @Setter
  private SupervisoryNode supervisoryNode;

  @OneToMany
  @JoinColumn(name = "requisitionGroupProgramSchedulesId")
  @Getter
  @Setter
  private List<RequisitionGroupProgramSchedule> requisitionGroupProgramSchedules;

  @OneToMany
  @JoinColumn(name = "facilityId")
  @Getter
  @Setter
  private List<Facility> memberFacilites;
}
