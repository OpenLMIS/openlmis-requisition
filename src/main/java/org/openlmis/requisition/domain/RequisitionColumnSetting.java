package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "available_requisition_column_setting", schema = "requisition")
@RequiredArgsConstructor
@Getter
@Setter
public class RequisitionColumnSetting extends BaseEntity {

  @JsonIgnore
  @OneToOne
  @JoinColumn(name = "columnId", nullable = false)
  private AvailableRequisitionColumn requisitionColumn;

  @Column(nullable = false)
  private String name;

  @Column
  private String value;

}
