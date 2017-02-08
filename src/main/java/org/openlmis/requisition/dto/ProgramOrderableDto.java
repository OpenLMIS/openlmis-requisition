package org.openlmis.requisition.dto;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import org.joda.money.Money;
import org.openlmis.utils.MoneyDeserializer;
import org.openlmis.utils.MoneySerializer;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class ProgramOrderableDto {
  private UUID programId;
  private UUID orderableId;
  private UUID orderableDisplayCategoryId;
  private String orderableCategoryDisplayName;
  private Integer orderableCategoryDisplayOrder;
  private Boolean active;
  private Boolean fullSupply;
  private Integer displayOrder;
  private Integer dosesPerPatient;
  @JsonSerialize(using = MoneySerializer.class)
  @JsonDeserialize(using = MoneyDeserializer.class)
  private Money pricePerPack;
}
