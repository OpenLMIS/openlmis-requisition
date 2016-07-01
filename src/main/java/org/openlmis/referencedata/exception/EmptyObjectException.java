package org.openlmis.referencedata.exception;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
public class EmptyObjectException extends RuntimeException {

  private String errorMessage;
}
