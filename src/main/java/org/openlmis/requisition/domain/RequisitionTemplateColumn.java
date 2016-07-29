package org.openlmis.requisition.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
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

  private Boolean canBeChangedByUser;

  private SourceType source;

  /**
   * Validate name of new label and change it if it's ok.
   * @return result of validation
   */
  public boolean changeLabel(String labelName) {
    if (validateString(labelName)) {
      this.label = labelName;
      return true;
    }
    return false;
  }

  private boolean validateString(String string) {
    Pattern pattern = Pattern.compile("^[a-zA-z0-9]+[a-zA-Z0-9 ]+$");
    Matcher matcher = pattern.matcher(string);
    return matcher.find();
  }

  /**
   * Allows changing visibility of specific column.
   * Modifies display order if column represents product code.
   *
   * @param isDisplayed Should the column be displayed.
   */
  public void setIsDisplayed(boolean isDisplayed) {
    if (this.name.equals("productCode")) {
      this.displayOrder = 1;
    }
    this.isDisplayed = isDisplayed;
  }
}