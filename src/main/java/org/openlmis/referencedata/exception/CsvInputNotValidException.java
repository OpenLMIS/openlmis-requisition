package org.openlmis.referencedata.exception;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
public class CsvInputNotValidException extends RuntimeException {

  public CsvInputNotValidException(String errorMessage, Exception exception) {
    super(errorMessage + " - " + exception.getMessage());
  }

  private String errorMessage;
}
