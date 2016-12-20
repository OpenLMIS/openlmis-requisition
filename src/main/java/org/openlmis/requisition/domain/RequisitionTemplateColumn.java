package org.openlmis.requisition.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Embeddable;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;


@Embeddable
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class RequisitionTemplateColumn {

  private String name;

  private String label;

  private String indicator;

  private int displayOrder;

  private Boolean isDisplayed;

  private SourceType source;

  @ManyToOne
  @JoinColumn(name = "requisitionColumnId", nullable = false)
  private AvailableRequisitionColumn columnDefinition;

  @ManyToOne
  @JoinColumn(name = "requisitionColumnOptionId")
  private AvailableRequisitionColumnOption option;

  public RequisitionTemplateColumn(AvailableRequisitionColumn columnDefinition) {
    this.columnDefinition = columnDefinition;
  }

  /**
   * Validate name of new label and change it if it's alphanumeric.
   * @throws RequisitionTemplateColumnException Exception thrown when
   *      given label name is not alphanumeric.
   */
  public void setLabel(String labelName) throws RequisitionTemplateColumnException {
    if (!validateString(labelName)) {
      throw new RequisitionTemplateColumnException("Only alphanumeric label is accepted.");
    }
    this.label = labelName;
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

  private boolean validateString(String string) {
    Pattern pattern = Pattern.compile("^[a-zA-z0-9/]+[a-zA-Z0-9/ ]+$");
    Matcher matcher = pattern.matcher(string);
    return matcher.find();
  }
}
