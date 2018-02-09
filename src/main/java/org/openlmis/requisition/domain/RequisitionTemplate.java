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

import static org.apache.commons.lang3.BooleanUtils.isNotTrue;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_ASSIGN_TEMPLATE_TO_SEVERAL_PROGRAMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMNS_MAP_IS_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_NOT_IN_TEMPLATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN;

import com.google.common.collect.Sets;

import org.javers.core.metamodel.annotation.DiffIgnore;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.utils.Message;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.MapKeyColumn;
import javax.persistence.OneToMany;
import javax.persistence.PostLoad;
import javax.persistence.Table;
import javax.persistence.Transient;

@SuppressWarnings("PMD.TooManyMethods")
@Entity
@Table(name = "requisition_templates")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class RequisitionTemplate extends BaseTimestampedEntity {

  public static final String SOURCE = "Source ";
  public static final String OPTION = "Option ";
  public static final String WARNING_SUFFIX = " is not available for this column.";

  @Getter
  @Setter
  private Integer numberOfPeriodsToAverage;

  @Getter
  @Setter
  private boolean populateStockOnHandFromStockCards;

  @Getter
  @Setter
  private String name;

  @Getter
  @Setter
  @ElementCollection(fetch = FetchType.LAZY)
  @MapKeyColumn(name = "key")
  @Column(name = "value")
  @CollectionTable(
      name = "columns_maps",
      joinColumns = @JoinColumn(name = "requisitionTemplateId"))
  private Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<>();

  @OneToMany(
      cascade = CascadeType.ALL,
      orphanRemoval = true,
      mappedBy = "template")
  @DiffIgnore
  private Set<RequisitionTemplateAssignment> templateAssignments = new HashSet<>();

  @Transient
  @Getter
  private UUID programId;

  @Transient
  @Getter
  private Set<UUID> facilityTypeIds = Sets.newHashSet();

  RequisitionTemplate(UUID id) {
    setId(id);
  }

  /**
   * Allows creating requisition template with the given properties.
   */
  public RequisitionTemplate(Integer numberOfPeriodsToAverage,
                             boolean populateStockOnHandFromStockCards,
                             String name,
                             Map<String, RequisitionTemplateColumn> columnsMap,
                             Set<RequisitionTemplateAssignment> templateAssignments) {
    this.numberOfPeriodsToAverage = numberOfPeriodsToAverage;
    this.populateStockOnHandFromStockCards = populateStockOnHandFromStockCards;
    this.name = name;
    this.columnsMap = columnsMap;

    setTemplateAssignments(templateAssignments);
  }

  /**
   * Allows creating requisition template with predefined columns.
   *
   * @param columns Columns to appear in requisition template.
   */
  public RequisitionTemplate(Map<String, RequisitionTemplateColumn> columns) {
    columns.forEach(columnsMap::put);
  }

  @PostLoad
  private void postLoad() {
    for (RequisitionTemplateAssignment assignment : templateAssignments) {
      setProgramId(assignment.getProgramId());
      addFacilityTypeId(assignment.getFacilityTypeId());
    }
  }

  /**
   * Add new assignment. Currently template can be assign to single program and several facility
   * types.
   */
  public void addAssignment(UUID programId, UUID facilityTypeId) {
    setProgramId(programId);
    addFacilityTypeId(facilityTypeId);
    templateAssignments.add(new RequisitionTemplateAssignment(programId, facilityTypeId, this));
  }

  private synchronized void setProgramId(UUID programId) {
    if (null == this.programId) {
      this.programId = programId;
    }

    if (!Objects.equals(this.programId, programId)) {
      throw new ValidationMessageException(ERROR_CANNOT_ASSIGN_TEMPLATE_TO_SEVERAL_PROGRAMS);
    }
  }

  private synchronized void addFacilityTypeId(UUID facilityTypeId) {
    Optional.ofNullable(facilityTypeId).ifPresent(id -> facilityTypeIds.add(id));
  }

  /**
   * Checks if column with given name is displayed.
   *
   * @param name name of requisition column.
   * @return return true if column is displayed
   */
  public boolean isColumnDisplayed(String name) {
    RequisitionTemplateColumn column = findColumn(name);

    return column.getIsDisplayed();
  }

  /**
   * Checks if column with given name is calculated.
   *
   * @param name name of requisition column.
   * @return return true if column is calculated
   */
  public boolean isColumnCalculated(String name) {
    RequisitionTemplateColumn column = findColumn(name);

    return SourceType.CALCULATED.equals(column.getSource());
  }

  /**
   * Checks if column with given name is input by user.
   *
   * @param name name of requisition column.
   * @return return true if column is calculated
   */
  public boolean isColumnUserInput(String name) {
    RequisitionTemplateColumn column = findColumn(name);

    return SourceType.USER_INPUT.equals(column.getSource());
  }

  /**
   * Allows changing the display order of columns.
   *
   * @param key             Key to column which needs a new display order.
   * @param newDisplayOrder Number specifying new display order of extracted column.
   */
  public void changeColumnDisplayOrder(String key, int newDisplayOrder) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    Integer oldDisplayOrder = column.getDisplayOrder();
    if (oldDisplayOrder == null) {
      moveDownAllColumnsBelowIndex(newDisplayOrder);
    } else {
      if (newDisplayOrder > oldDisplayOrder) {
        moveUpAllColumnsBetweenIndexes(newDisplayOrder, oldDisplayOrder);
      } else {
        moveDownAllColumnsBetweenIndexes(newDisplayOrder, oldDisplayOrder);
      }
    }
    if (column.getColumnDefinition().getCanChangeOrder()) {
      column.setDisplayOrder(newDisplayOrder);
    }
  }

  /**
   * @param key     Key to column which needs a new display property.
   * @param display Should column be displayed.
   */
  public void changeColumnDisplay(String key, boolean display) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    if (isNotTrue(column.getColumnDefinition().getIsDisplayRequired())) {
      if (display && "productCode".equals(key)) {
        column.setDisplayOrder(1);
      }
      column.setIsDisplayed(display);
    }
  }

  /**
   * @param key  Key to column which needs a new name.
   * @param name New name for label.
   */
  public void changeColumnLabel(String key, String name) {
    RequisitionTemplateColumn column = columnsMap.get(key);
    column.setLabel(name);
  }

  /**
   * Validate source of column and change it if it's available.
   *
   * @param key    Key to column which needs a new source.
   * @param source New source for column.
   */
  public void changeColumnSource(String key, SourceType source) {

    RequisitionTemplateColumn column = findColumn(key);

    if (column.getColumnDefinition().getSources() == null) {
      throw new ValidationMessageException(new Message(ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN,
          source.toString()));
    }

    if (!column.getColumnDefinition().getSources().contains(source)) {
      throw new ValidationMessageException(new Message(ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN,
          source.toString()));
    }
    column.setSource(source);
  }

  /**
   * Validate option of column and change it if it's available.
   *
   * @param key    Key to column which needs a new option.
   * @param option New option for column.
   */
  public void changeColumnOption(String key, AvailableRequisitionColumnOption option) {

    RequisitionTemplateColumn column = findColumn(key);

    if (column.getColumnDefinition().getOptions() == null) {
      throw new ValidationMessageException(new Message(ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN,
          option.getOptionName()));
    }

    if (!column.getColumnDefinition().getOptions().contains(option)) {
      throw new ValidationMessageException(new Message(ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN,
          option.getOptionName()));
    }
    column.setOption(option);
  }

  /**
   * Copy values of attributes into new or updated RequisitionTemplate.
   *
   * @param requisitionTemplate RequisitionTemplate with new values.
   */
  public void updateFrom(RequisitionTemplate requisitionTemplate) {
    this.numberOfPeriodsToAverage = requisitionTemplate.getNumberOfPeriodsToAverage();
    this.name = requisitionTemplate.name;

    requisitionTemplate.getColumnsMap().forEach(columnsMap::put);
    setTemplateAssignments(requisitionTemplate.templateAssignments);
  }

  private void setTemplateAssignments(Set<RequisitionTemplateAssignment> assignments) {
    for (RequisitionTemplateAssignment assignment : assignments) {
      addAssignment(assignment.getProgramId(), assignment.getFacilityTypeId());
    }
  }

  public boolean hasColumnsDefined() {
    return columnsMap != null && !columnsMap.isEmpty();
  }

  /**
   * Checks if column with given name is defined in the template.
   *
   * @param columnName name of requisition column.
   * @return return true if column is defined in the template.
   */
  public boolean isColumnInTemplate(String columnName) {
    return getRequisitionTemplateColumn(columnName) != null;
  }


  /**
   * Checks if column with given name is defined in the template and displayed.
   *
   * @param columnName name of requisition column.
   * @return return true if column is defined in the template and displayed.
   */
  public boolean isColumnInTemplateAndDisplayed(String columnName) {
    return isColumnInTemplate(columnName) && isColumnDisplayed(columnName);
  }

  private void moveDownAllColumnsBelowIndex(int beginIndex) {
    for (RequisitionTemplateColumn column : columnsMap.values()) {
      if (column.getDisplayOrder() >= beginIndex) {
        column.setDisplayOrder(column.getDisplayOrder() + 1);
      }
    }
  }

  private void moveUpAllColumnsBetweenIndexes(int beginIndex, int endIndex) {
    for (RequisitionTemplateColumn column : columnsMap.values()) {
      if (column.getDisplayOrder() <= beginIndex && column.getDisplayOrder() > endIndex) {
        column.setDisplayOrder(column.getDisplayOrder() - 1);
      }
    }
  }

  private void moveDownAllColumnsBetweenIndexes(int beginIndex, int endIndex) {
    for (RequisitionTemplateColumn column : columnsMap.values()) {
      if (column.getDisplayOrder() >= beginIndex && column.getDisplayOrder() < endIndex) {
        column.setDisplayOrder(column.getDisplayOrder() + 1);
      }
    }
  }

  /**
   * Finds a column by column name or throws exception.
   *
   * @param name name of requisition column.
   * @return {c@link RequisitionTemplateColumn} if found column with the given name.
   */
  public RequisitionTemplateColumn findColumn(String name) {
    RequisitionTemplateColumn column = getRequisitionTemplateColumn(name);
    if (column == null) {
      throw new ValidationMessageException(new Message(ERROR_COLUMN_NOT_IN_TEMPLATE, name));
    }
    return column;
  }

  private RequisitionTemplateColumn getRequisitionTemplateColumn(String name) {
    if (columnsMap == null) {
      throw new ValidationMessageException(new Message(ERROR_COLUMNS_MAP_IS_NULL));
    }
    return columnsMap.get(name);
  }

  /**
   * Create a new instance of requisiton template based on data
   * from {@link RequisitionTemplate.Importer}
   *
   * @param importer instance of {@link RequisitionTemplate.Importer}
   * @return new instance od template.
   */
  public static RequisitionTemplate newInstance(RequisitionTemplate.Importer importer) {
    RequisitionTemplate template = new RequisitionTemplate();
    template.setId(importer.getId());
    template.setCreatedDate(importer.getCreatedDate());
    template.setModifiedDate(importer.getModifiedDate());
    template.setPopulateStockOnHandFromStockCards(
        importer.isPopulateStockOnHandFromStockCards());
    template.setNumberOfPeriodsToAverage(importer.getNumberOfPeriodsToAverage());
    template.setColumnsMap(new HashMap<>());
    template.setName(importer.getName());

    importer.getColumnsMap()
        .forEach((key, column) ->
            template.getColumnsMap()
                .put(key, RequisitionTemplateColumn.newInstance(column)));

    if (importer.getFacilityTypeIds().isEmpty()) {
      template.addAssignment(importer.getProgramId(), null);
    } else {
      for (UUID facilityTypeId : importer.getFacilityTypeIds()) {
        template.addAssignment(importer.getProgramId(), facilityTypeId);
      }
    }

    return template;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(RequisitionTemplate.Exporter exporter) {
    exporter.setId(id);
    exporter.setCreatedDate(getCreatedDate());
    exporter.setModifiedDate(getModifiedDate());
    exporter.setPopulateStockOnHandFromStockCards(populateStockOnHandFromStockCards);
    exporter.setNumberOfPeriodsToAverage(numberOfPeriodsToAverage);
    exporter.setProgramId(getProgramId());
    exporter.setFacilityTypeIds(getFacilityTypeIds());
  }

  public interface Importer {
    UUID getId();

    ZonedDateTime getCreatedDate();

    ZonedDateTime getModifiedDate();

    boolean isPopulateStockOnHandFromStockCards();

    String getName();

    Integer getNumberOfPeriodsToAverage();

    Map<String, ? extends RequisitionTemplateColumn.Importer> getColumnsMap();

    UUID getProgramId();

    Set<UUID> getFacilityTypeIds();
  }

  public interface Exporter {
    void setId(UUID id);

    void setCreatedDate(ZonedDateTime createdDate);

    void setModifiedDate(ZonedDateTime modifiedDate);

    void setPopulateStockOnHandFromStockCards(boolean populateStockOnHandFromStockCards);

    void setNumberOfPeriodsToAverage(Integer numberOfPeriodsToAverage);

    void setName(String name);

    void setProgramId(UUID programId);

    void setFacilityTypeIds(Set<UUID> facilityTypeIds);
  }
}
