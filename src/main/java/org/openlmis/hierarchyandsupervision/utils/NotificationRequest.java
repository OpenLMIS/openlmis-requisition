package org.openlmis.hierarchyandsupervision.utils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
public class NotificationRequest {

  @Getter
  @Setter
  private String from;

  @Getter
  @Setter
  private String to;

  @Getter
  @Setter
  private String subject;

  @Getter
  @Setter
  private String content;

  @Getter
  @Setter
  private String htmlContent;
}