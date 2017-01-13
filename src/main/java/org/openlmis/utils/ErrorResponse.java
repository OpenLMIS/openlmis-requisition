package org.openlmis.utils;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
public class ErrorResponse {

  @Getter
  private String message;

  @Getter
  private String description;
}
