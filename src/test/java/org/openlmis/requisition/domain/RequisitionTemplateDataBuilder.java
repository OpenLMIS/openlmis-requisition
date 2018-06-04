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

package org.openlmis.requisition.domain;

import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.BEGINNING_BALANCE;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.REQUESTED_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_RECEIVED_QUANTITY;

import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.javers.common.collections.Sets;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.openlmis.requisition.validate.RequisitionValidationTestUtils;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTemplateDataBuilder {
  private static int instanceNumber = 0;

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
    instanceNumber++;

    id = UUID.randomUUID();
    createdDate = ZonedDateTime.now();
    modifiedDate = ZonedDateTime.now();
    numberOfPeriodsToAverage = 6;
    populateStockOnHandFromStockCards = false;
    name = "template-name-" + instanceNumber;
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

  /**
   * Sets all initiate columns.
   */
  public RequisitionTemplateDataBuilder withAllColumns() {
    return withColumns(RequisitionValidationTestUtils.initiateColumns());
  }

  /**
   * Sets all initiate columns but without stock on hand column.
   */
  public RequisitionTemplateDataBuilder withAllColumnsExceptStockOnHand() {
    withAllColumns();
    columnsMap.remove(RequisitionLineItem.STOCK_ON_HAND);
    return this;
  }

  /**
   * Sets all initiate columns but without total consumed quantity column.
   */
  public RequisitionTemplateDataBuilder withAllColumnsExceptTotalConsumedQuantity() {
    withAllColumns();
    columnsMap.remove(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    return this;
  }

  /**
   * Sets all initiate columns and hide stock on hand column.
   */
  public RequisitionTemplateDataBuilder withStockOnHandColumnHiden() {
    withAllColumns();
    columnsMap.get(RequisitionLineItem.STOCK_ON_HAND).setIsDisplayed(false);
    return this;
  }

  /**
   * Sets all initiate columns and set calculated source for stock on hand column.
   */
  public RequisitionTemplateDataBuilder withStockOnHandColumnCalculated() {
    withAllColumns();
    columnsMap.get(RequisitionLineItem.STOCK_ON_HAND).setSource(SourceType.CALCULATED);
    return this;
  }

  /**
   * Sets all initiate columns and hide total consumed quantity column.
   */
  public RequisitionTemplateDataBuilder withTotalConsumedQuantityColumnHidden() {
    withAllColumns();
    columnsMap.get(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY).setIsDisplayed(false);
    return this;
  }

  /**
   * Set populateStockOnHandFromStockCards flag. If builder contains stock based columns,
   * the method will modify the source to {@link SourceType#STOCK_CARDS}.
   */
  public RequisitionTemplateDataBuilder withPopulateStockOnHandFromStockCards() {
    populateStockOnHandFromStockCards = true;

    for (String stockColumn : Arrays.asList(BEGINNING_BALANCE, STOCK_ON_HAND,
        TOTAL_CONSUMED_QUANTITY, TOTAL_RECEIVED_QUANTITY, TOTAL_LOSSES_AND_ADJUSTMENTS)) {
      if (columnsMap.containsKey(stockColumn)) {
        columnsMap.get(stockColumn).setSource(SourceType.STOCK_CARDS);
      }
    }

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
   * Adds columns to the {@link RequisitionTemplate} with a duplicated tag.
   */
  public RequisitionTemplateDataBuilder withDuplicatedTag() {
    return this
        .withColumn(CALCULATED_ORDER_QUANTITY, "I", SourceType.CALCULATED, null,
            Sets.asSet(SourceType.CALCULATED), "tag1")
        .withColumn(REQUESTED_QUANTITY, "J", SourceType.USER_INPUT, null,
            Sets.asSet(SourceType.USER_INPUT))
        .withColumn(REQUESTED_QUANTITY_EXPLANATION, "W", SourceType.USER_INPUT, null,
            Sets.asSet(SourceType.USER_INPUT), "tag1");
  }

  public RequisitionTemplateDataBuilder withColumn(String name, String indicator,
                                                   SourceType source,
                                                   Set<SourceType> sources) {
    return withColumn(name, indicator, source, null, sources, null);
  }

  public RequisitionTemplateDataBuilder withColumn(String name, String indicator,
      SourceType source, AvailableRequisitionColumnOption option, Set<SourceType> sources) {
    return withColumn(name, indicator, source, option, sources, null);
  }

  /**
   * Adds column to the columns map for new {@link RequisitionTemplate} instance.
   */
  public RequisitionTemplateDataBuilder withColumn(String name, String indicator,
                                                   SourceType source,
                                                   AvailableRequisitionColumnOption option,
                                                   Set<SourceType> sources,
                                                   String tag) {
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
        .withTag(tag)
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
