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

import static org.openlmis.requisition.domain.BaseEntity.TEXT_COLUMN_DEFINITION;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.dto.AvailableRequisitionColumnDto;
import org.openlmis.requisition.dto.AvailableRequisitionColumnOptionDto;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

@Embeddable
@NoArgsConstructor
@AllArgsConstructor
public class RequisitionTemplateColumn {
  public static final String COLUMN_DEFINITION = "columnDefinition";
  public static final String DEFINITION = "definition";

  @Getter
  @Setter
  private String name;

  @Getter
  @Setter
  private String label;

  @Getter
  @Setter
  private String indicator;

  @Getter
  @Setter
  private int displayOrder;

  @Getter
  private Boolean isDisplayed;

  @Column(nullable = false)
  @Getter
  @Setter
  private SourceType source;

  @ManyToOne(
      cascade = {CascadeType.REFRESH},
      fetch = FetchType.EAGER)
  @JoinColumn(name = "requisitionColumnId", nullable = false)
  @Getter
  @Setter
  private AvailableRequisitionColumn columnDefinition;

  @ManyToOne(
      cascade = {CascadeType.REFRESH},
      fetch = FetchType.EAGER)
  @JoinColumn(name = "requisitionColumnOptionId")
  @Getter
  @Setter
  private AvailableRequisitionColumnOption option;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String definition;

  public RequisitionTemplateColumn(AvailableRequisitionColumn columnDefinition) {
    this.columnDefinition = columnDefinition;
  }

  /**
   * Allows changing visibility of specific column.
   * Modifies display order if column represents product code.
   *
   * @param isDisplayed Should the column be displayed.
   */
  public void setIsDisplayed(boolean isDisplayed) {
    if (this.name.equals("productCode")) {
      this.displayOrder = 1;
    }
    this.isDisplayed = isDisplayed;
  }

  /**
   * Create a new instance of requisition template column based on data
   * from {@link RequisitionTemplateColumn.Importer}
   *
   * @param importer instance of {@link RequisitionTemplateColumn.Importer}
   * @return new instance od template column.
   */
  public static RequisitionTemplateColumn newInstance(RequisitionTemplateColumn.Importer importer) {
    RequisitionTemplateColumn requisitionTemplateColumn = new RequisitionTemplateColumn();
    requisitionTemplateColumn.setName(importer.getName());
    requisitionTemplateColumn.setLabel(importer.getLabel());
    requisitionTemplateColumn.setIndicator(importer.getIndicator());
    requisitionTemplateColumn.setDisplayOrder(importer.getDisplayOrder());
    requisitionTemplateColumn.setIsDisplayed(importer.getIsDisplayed());
    requisitionTemplateColumn.setSource(importer.getSource());
    requisitionTemplateColumn.setColumnDefinition(
        AvailableRequisitionColumn.newInstance(importer.getColumnDefinition()));
    requisitionTemplateColumn.setOption(
        AvailableRequisitionColumnOption.newInstance(importer.getOption()));
    requisitionTemplateColumn.setDefinition(importer.getDefinition());

    return requisitionTemplateColumn;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(RequisitionTemplateColumn.Exporter exporter) {
    exporter.setName(name);
    exporter.setLabel(label);
    exporter.setIndicator(indicator);
    exporter.setDisplayOrder(displayOrder);
    exporter.setIsDisplayed(isDisplayed);
    exporter.setSource(source);
    exporter.setColumnDefinition(AvailableRequisitionColumnDto.newInstance(columnDefinition));
    exporter.setOption(AvailableRequisitionColumnOptionDto.newInstance(option));
    exporter.setDefinition(definition);
  }

  public interface Importer {
    String getName();

    String getLabel();

    String getIndicator();

    int getDisplayOrder();

    Boolean getIsDisplayed();

    SourceType getSource();

    AvailableRequisitionColumnDto getColumnDefinition();

    AvailableRequisitionColumnOptionDto getOption();

    String getDefinition();
  }

  public interface Exporter {
    void setName(String name);

    void setLabel(String label);

    void setIndicator(String indicator);

    void setDisplayOrder(int displayOrder);

    void setIsDisplayed(Boolean isDisplayed);

    void setSource(SourceType source);

    void setColumnDefinition(AvailableRequisitionColumnDto columnDefinition);

    void setOption(AvailableRequisitionColumnOptionDto option);

    void setDefinition(String definition);
  }

}
