package org.openlmis.fulfillment.domain;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "orderNumberConfiguration")
@EqualsAndHashCode(callSuper = false)
public class OrderNumberConfiguration extends BaseEntity {

  @Getter
  @Setter
  @Column
  private String orderNumberPrefix;

  @Getter
  @Setter
  @Column
  private Boolean includeOrderNumberPrefix;

  @Getter
  @Setter
  @Column
  private Boolean includeProgramCode;

  @Getter
  @Setter
  @Column
  private Boolean includeRequisitionTypeSuffix;

  /**
   * Generates order number for given parameters.
   * @param id UUID of requisition from which order have been converted.
   * @param programCode Code from the Program associated with the order.
   * @param emergency Boolean indicates if requisition is emergency.
   * @return Generated orderNumber.
   */
  public String generateOrderNumber(UUID id, String programCode, Boolean emergency) {
    StringBuilder orderNumber = new StringBuilder();

    if (includeOrderNumberPrefix && orderNumberPrefix != null) {
      orderNumber.append(getOrderNumberPrefix());
    }
    if (includeProgramCode) {
      orderNumber.append(getTruncatedProgramCode(programCode));
    }
    orderNumber.append(id.toString());
    if (includeRequisitionTypeSuffix) {
      orderNumber.append(emergency ? "E" : "R");
    }
    return orderNumber.toString();
  }

  private String getTruncatedProgramCode(String code) {
    return code.length() > 35 ? code.substring(0, 35) : code;
  }

}
