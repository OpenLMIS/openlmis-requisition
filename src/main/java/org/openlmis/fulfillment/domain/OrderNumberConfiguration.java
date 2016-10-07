package org.openlmis.fulfillment.domain;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.fulfillment.exception.OrderNumberException;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ProgramDto;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "order_number_configurations")
@EqualsAndHashCode(callSuper = false)
public class OrderNumberConfiguration extends BaseEntity {

  @Getter
  @Setter
  @Column
  private String orderNumberPrefix;

  @Getter
  @Setter
  @Column(nullable = false)
  private Boolean includeOrderNumberPrefix;

  @Getter
  @Setter
  @Column(nullable = false)
  private Boolean includeProgramCode;

  @Getter
  @Setter
  @Column(nullable = false)
  private Boolean includeRequisitionTypeSuffix;

  /**
   * Generates order number for given parameters.
   * @param requisition Requisition from which order have been converted.
   * @param program Program associated with the order.
   * @return Generated orderNumber.
   */
  public String generateOrderNumber(Requisition requisition, ProgramDto program)
      throws OrderNumberException {
    StringBuilder orderNumber = new StringBuilder();
    if (requisition == null) {
      throw new OrderNumberException("Requisition cannot be empty");
    }
    if (includeOrderNumberPrefix && orderNumberPrefix != null) {
      orderNumber.append(getOrderNumberPrefix());
    }
    if (includeProgramCode && program != null) {
      orderNumber.append(getTruncatedProgramCode(program.getCode()));
    }
    orderNumber.append(requisition.getId().toString());
    if (includeRequisitionTypeSuffix) {
      orderNumber.append(requisition.getEmergency() ? "E" : "R");
    }
    return orderNumber.toString();
  }

  private String getTruncatedProgramCode(String code) {
    return code.length() > 35 ? code.substring(0, 35) : code;
  }
}
