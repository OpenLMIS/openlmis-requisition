package org.openlmis.requisition.domain;

import static org.openlmis.requisition.domain.BaseEntity.TEXT_COLUMN_DEFINITION;

import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.utils.Message;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

@Embeddable
@NoArgsConstructor
@AllArgsConstructor
public class RequisitionTemplateColumn {
  public static final String DEFINITION = "definition";

  @Getter
  @Setter
  private String name;

  @Getter
  private String label;

  @Getter
  @Setter
  private String indicator;

  @Getter
  @Setter
  private int displayOrder;

  @Getter
  private Boolean isDisplayed;

  @Getter
  @Setter
  private SourceType source;

  @ManyToOne(
      cascade = {CascadeType.REFRESH},
      fetch = FetchType.EAGER)
  @JoinColumn(name = "requisitionColumnId", nullable = false)
  @Getter
  @Setter
  private AvailableRequisitionColumn columnDefinition;

  @ManyToOne(
      cascade = {CascadeType.REFRESH},
      fetch = FetchType.EAGER)
  @JoinColumn(name = "requisitionColumnOptionId")
  @Getter
  @Setter
  private AvailableRequisitionColumnOption option;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String definition;

  public RequisitionTemplateColumn(AvailableRequisitionColumn columnDefinition) {
    this.columnDefinition = columnDefinition;
  }

  /**
   * Validate name of new label and change it if it's alphanumeric.
   */
  public void setLabel(String labelName) {
    if (!validateString(labelName)) {
      throw new ValidationMessageException(
          new Message("requisition.error.only-alphanumeric-label-is-accepted"));
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
