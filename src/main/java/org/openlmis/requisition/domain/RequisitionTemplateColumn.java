package org.openlmis.requisition.domain;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Embeddable;

@Embeddable
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class RequisitionTemplateColumn {

  private String name;

  private String label;

  private int displayOrder;

  private Boolean isDisplayed;

  private Boolean isDisplayRequired;

  private Boolean canChangeOrder;

  private String source; //todo change String to SourceType {User Input, Reference Data, Calculated}

  public void setIsDisplayed(boolean isDisplayed) {
    if (this.name.equals("productCode")) {
      this.displayOrder = 1;
    }
    this.isDisplayed = isDisplayed;
  }
}