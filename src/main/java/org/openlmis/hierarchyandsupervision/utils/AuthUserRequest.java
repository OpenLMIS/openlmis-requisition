package org.openlmis.hierarchyandsupervision.utils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AuthUserRequest {
  private static final String DEFAULT_ROLE = "USER";

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private UUID referenceDataUserId;

  @Getter
  @Setter
  private String username;

  @Getter
  @Setter
  private String password;

  @Getter
  @Setter
  private String email;

  @Getter
  @Setter
  private String role = DEFAULT_ROLE;

  @Getter
  @Setter
  private Boolean enabled = true;
}
