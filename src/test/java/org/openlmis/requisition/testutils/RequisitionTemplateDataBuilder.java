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

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.javers.common.collections.Sets;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class RequisitionTemplateDataBuilder {

  private UUID id;
  private ZonedDateTime createdDate;
  private ZonedDateTime modifiedDate;
  private Integer numberOfPeriodsToAverage;
  private String name;
  private boolean populateStockOnHandFromStockCards;
  private Map<String, RequisitionTemplateColumn> columnsMap;
  private Set<Pair<UUID, UUID>> templateAssignments;

  /**
   * Builder for {@link RequisitionTemplate} class.
   */
  public RequisitionTemplateDataBuilder() {
    id = UUID.randomUUID();
    createdDate = ZonedDateTime.now();
    modifiedDate = ZonedDateTime.now();
    numberOfPeriodsToAverage = 6;
    populateStockOnHandFromStockCards = false;
    name = "template-name-" + RandomStringUtils.randomNumeric(25);
    columnsMap = new HashMap<>();

    templateAssignments = new HashSet<>();
  }

  public RequisitionTemplateDataBuilder withAssignment(UUID programId, UUID facilityTypeId) {
    templateAssignments.add(new ImmutablePair<>(programId, facilityTypeId));
    return this;
  }

  /**
   * Builds {@link RequisitionTemplate} instance with test data.
   */
  public RequisitionTemplate build() {
    RequisitionTemplate template = new RequisitionTemplate(id, numberOfPeriodsToAverage,
        populateStockOnHandFromStockCards, name, columnsMap, new HashSet<>());
    template.setCreatedDate(createdDate);
    template.setModifiedDate(modifiedDate);

    for (Pair<UUID, UUID> assignment : templateAssignments) {
      template.addAssignment(assignment.getLeft(), assignment.getRight());
    }

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

  public RequisitionTemplateDataBuilder withColumn(String name, String indicator,
                                                   SourceType source,
                                                   Set<SourceType> sources) {
    return withColumn(name, indicator, source, null, sources);
  }

  /**
   * Adds column to the columns map for new {@link RequisitionTemplate} instance.
   */
  public RequisitionTemplateDataBuilder withColumn(String name, String indicator,
                                                   SourceType source,
                                                   AvailableRequisitionColumnOption option,
                                                   Set<SourceType> sources) {
    columnsMap.put(name, new RequisitionTemplateColumnDataBuilder()
        .withName(name)
        .withIndicator(indicator)
        .withDisplayOrder(columnsMap.size() + 1)
        .withOption(option)
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(name)
            .withSources(sources)
            .build())
        .withSource(source)
        .build());
    return this;
  }

  public RequisitionTemplateDataBuilder withColumns(
      Map<String, RequisitionTemplateColumn> columnsMap) {
    this.columnsMap = columnsMap;
    return this;
  }

  public RequisitionTemplateDataBuilder withoutId() {
    this.id = null;
    return this;
  }

  public RequisitionTemplateDataBuilder withName(String name) {
    this.name = name;
    return this;
  }
}
