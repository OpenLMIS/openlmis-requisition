/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.testutils;

import static org.openlmis.requisition.domain.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.REQUESTED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION;

import org.javers.common.collections.Sets;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class RequisitionTemplateDataBuilder {

  private UUID id;
  private ZonedDateTime createdDate;
  private ZonedDateTime modifiedDate;
  private UUID programId;
  private Integer numberOfPeriodsToAverage;
  private boolean populateStockOnHandFromStockCards;
  private Map<String, RequisitionTemplateColumn> columnsMap;

  /**
   * Builder for {@link RequisitionTemplate} class.
   */
  public RequisitionTemplateDataBuilder() {
    id = UUID.randomUUID();
    createdDate = ZonedDateTime.now();
    modifiedDate = ZonedDateTime.now();
    programId = UUID.randomUUID();
    numberOfPeriodsToAverage = 6;
    populateStockOnHandFromStockCards = false;
    columnsMap = new HashMap<>();
  }

  /**
   * Builds {@link RequisitionTemplate} instance with test data.
   */
  public RequisitionTemplate build() {
    RequisitionTemplate template = new RequisitionTemplate(programId, numberOfPeriodsToAverage,
        populateStockOnHandFromStockCards, columnsMap);
    template.setId(id);
    template.setCreatedDate(createdDate);
    template.setModifiedDate(modifiedDate);
    return template;
  }

  public RequisitionTemplateDataBuilder withPopulateStockOnHandFromStockCards() {
    populateStockOnHandFromStockCards = true;
    return this;
  }

  public RequisitionTemplateDataBuilder withNumberOfPeriodsToAverage(Integer periodsToAverage) {
    numberOfPeriodsToAverage = periodsToAverage;
    return this;
  }

  /**
   * Adds columns that are required in {@link RequisitionTemplate} instance.
   */
  public RequisitionTemplateDataBuilder withRequiredColumns() {
    return this
        .withColumn(CALCULATED_ORDER_QUANTITY, "I", SourceType.CALCULATED,
            Sets.asSet(SourceType.CALCULATED))
        .withColumn(REQUESTED_QUANTITY, "J", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT))
        .withColumn(REQUESTED_QUANTITY_EXPLANATION, "W", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT));
  }

  /**
   * Adds column to the columns map for new {@link RequisitionTemplate} instance.
   */
  public RequisitionTemplateDataBuilder withColumn(String name, String indicator,
                                                   SourceType source, Set<SourceType> sources) {
    columnsMap.put(name, new RequisitionTemplateColumnDataBuilder()
        .withName(name)
        .withIndicator(indicator)
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(name)
            .withSources(sources)
            .build())
        .withSource(source)
        .build());
    return this;
  }
}
