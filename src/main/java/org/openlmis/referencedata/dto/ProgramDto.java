package org.openlmis.referencedata.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

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

  public ProgramDto() {
  }

  public ProgramDto(UUID id, String code, String name) {
    this.id = id;
    this.code = code;
    this.name = name;
  }
}
