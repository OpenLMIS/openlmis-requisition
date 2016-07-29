package org.openlmis.referencedata.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
public class ProgramDto {

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private String code;

  @Getter
  @Setter
  private String name;

}
