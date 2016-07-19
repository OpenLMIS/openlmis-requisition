package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.Set;

@Entity
@Table(name = "supervisory_nodes", schema = "referencedata")
@NoArgsConstructor
public class SupervisoryNode extends BaseEntity {
  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String code;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String description;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer supervisorCount;

  @ManyToOne
  @JoinColumn(nullable = false, name = "facilityid")
  @Getter
  @Setter
  private Facility facility;

  @ManyToOne
  @JoinColumn(name = "parentid")
  @Getter
  @Setter
  private SupervisoryNode parentNode;

  @OneToMany(mappedBy = "parentNode")
  @Getter
  @Setter
  private Set<SupervisoryNode> childNodes;
}
