package org.openlmis.requisition.domain;

import lombok.Getter;

public enum SourceType {
  REFERENCE("R", "Reference to object"),
  USER_INPUT("U", "Column source added by user"),
  CALCULATED("C", "Column source calculated by system");

  @Getter
  private final String code;

  @Getter
  private final String description;

  SourceType(String code, String description) {
    this.code = code;
    this.description = description;
  }
}
