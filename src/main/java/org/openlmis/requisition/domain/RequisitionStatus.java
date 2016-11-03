package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;

public enum RequisitionStatus {
  INITIATED(1),
  SUBMITTED(2),
  AUTHORIZED(3),
  APPROVED(4),
  RELEASED(5),
  SKIPPED(-1);

  private int value;

  RequisitionStatus(int value) {
    this.value = value;
  }

  @JsonIgnore
  public boolean isPreAuthorize() {
    return value == 1 || value == 2;
  }

  @JsonIgnore
  public boolean isPostSubmitted() {
    return value >= 2;
  }

}
