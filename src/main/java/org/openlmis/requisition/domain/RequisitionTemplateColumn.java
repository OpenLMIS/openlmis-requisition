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

  Boolean isDisplayed;

  Boolean isDisplayRequired;

  Boolean canChangeOrder;

  /**
   *
   * @param name Column name.
   * @param label Column label.
   * @param displayOrder Column position.
   */
  public RequisitionTemplateColumn(String name, String label, int displayOrder) {
    this.name = name;
    this.label = label;
    this.displayOrder = displayOrder;
    this.isDisplayed = true;
    this.isDisplayRequired = false;
    this.canChangeOrder = true;
  }

  /**
   *
   * @param name Column name.
   * @param label Column label.
   * @param displayOrder Column position.
   * @param isDisplayRequired Should column be always displayed.
   * @param canChangeOrder Should column always stay at the same position.
   * @param isDisplayed Is column currently displayed.
   */
  public RequisitionTemplateColumn(String name, String label, int displayOrder,
                                   Boolean isDisplayRequired, Boolean isDisplayed,
                                   Boolean canChangeOrder) {
    this.name = name;
    this.label = label;
    this.displayOrder = displayOrder;
    this.isDisplayRequired = isDisplayRequired;
    if (this.isDisplayRequired == true) {
      this.isDisplayed = true;
    } else {
      this.isDisplayed = isDisplayed;
    }
    this.canChangeOrder = canChangeOrder;
  }
}
