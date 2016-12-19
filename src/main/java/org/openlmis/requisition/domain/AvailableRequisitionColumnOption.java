package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "available_requisition_column_options", schema = "requisition")
@NoArgsConstructor
@AllArgsConstructor
public class AvailableRequisitionColumnOption extends BaseEntity {

  @ManyToOne(cascade = {CascadeType.REFRESH})
  @JoinColumn(name = "columnId", nullable = false)
  @JsonIgnore
  @Getter
  @Setter
  private AvailableRequisitionColumn requisitionColumn;

  @Column(nullable = false)
  @Getter
  @Setter
  private String optionName;

  @Column(nullable = false)
  @Getter
  @Setter
  private String optionLabel;
}
