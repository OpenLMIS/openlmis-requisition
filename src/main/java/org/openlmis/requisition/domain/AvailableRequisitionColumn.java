package org.openlmis.requisition.domain;

import java.util.Objects;

import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "available_requisition_columns", schema = "requisition")
@NoArgsConstructor
@Getter
@Setter
public class AvailableRequisitionColumn extends BaseEntity {
  private String name;
  private SourceType source;
  private String label;
  private boolean mandatory;

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }

    if (!(obj instanceof AvailableRequisitionColumn)) {
      return false;
    }

    AvailableRequisitionColumn column = (AvailableRequisitionColumn) obj;
    return Objects.equals(column.getName(), getName());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getName());
  }
}
