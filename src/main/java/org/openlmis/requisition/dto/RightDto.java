package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.Objects;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
public class RightDto {
  private UUID id;
  private String name;
  private RightType type;
  private String description;
  private Set<RightDto> attachments;

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof RightDto)) {
      return false;
    }
    RightDto rightDto = (RightDto) obj;
    return Objects.equals(name, rightDto.name);
  }
}
