package org.openlmis.requisition.domain;

import lombok.Getter;

public enum SourceType {
  REFERENCE("R", "label.column.source.reference.data"),
  USER_INPUT("U", "label.column.source.user.input"),
  CALCULATED("C", "label.column.source.calculated");

  @Getter
  private final String code;

  @Getter
  private final String description;

  SourceType(String code, String description) {
    this.code = code;
    this.description = description;
  }

  public static SourceType getValueOf(String value) {
    for (SourceType columnSource : SourceType.values()) {
      if (columnSource.code.equalsIgnoreCase(value)) {
        return columnSource;
      }
    }
    return null;
  }
}
