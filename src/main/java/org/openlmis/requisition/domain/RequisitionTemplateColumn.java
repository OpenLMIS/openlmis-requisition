package org.openlmis.requisition.domain;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Embeddable;

@Embeddable
@NoArgsConstructor
@Getter
@Setter
public class RequisitionTemplateColumn {
  String name;

  String label;

  int displayOrder;

  public RequisitionTemplateColumn(String name, String label, int displayOrder) {
    this.name = name;
    this.label = label;
    this.displayOrder = displayOrder;
  }
}
