package org.openlmis.referencedata.exception;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class ExceptionDetail {
  private String title;
  private int status;
  private String detail;
  private long timeStamp;
  private String developerMessage;
}