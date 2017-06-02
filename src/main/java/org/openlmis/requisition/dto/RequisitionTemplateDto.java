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

package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;
import org.openlmis.requisition.domain.RequisitionTemplate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Getter
@Setter
public class RequisitionTemplateDto extends BaseDto implements
    RequisitionTemplate.Importer, RequisitionTemplate.Exporter {

  private ZonedDateTime createdDate;

  private ZonedDateTime modifiedDate;

  private UUID programId;

  private Integer numberOfPeriodsToAverage;

  private Map<String, RequisitionTemplateColumnDto> columnsMap;

  /**
   * Create new list of RequisitionTemplateDto based on given list of {@link RequisitionTemplate}
   *
   * @param templates list of {@link RequisitionTemplate}
   * @return new list of RequisitionTemplateDto.
   */
  public static Iterable<RequisitionTemplateDto> newInstance(
      Iterable<RequisitionTemplate> templates) {

    List<RequisitionTemplateDto> requisitionTemplateDtos = new ArrayList<>();
    templates.forEach(t -> requisitionTemplateDtos.add(newInstance(t)));
    return requisitionTemplateDtos;
  }

  /**
   * Create new instance of RequisitionTemplateDto based on given {@link RequisitionTemplate}
   *
   * @param requisitionTemplate instance of Template
   * @return new instance of RequisitionTemplateDto.
   */
  public static RequisitionTemplateDto newInstance(RequisitionTemplate requisitionTemplate) {
    if (requisitionTemplate == null) {
      return null;
    }
    RequisitionTemplateDto requisitionTemplateDto = new RequisitionTemplateDto();
    requisitionTemplate.export(requisitionTemplateDto);
    requisitionTemplateDto.setColumnsMap(
        RequisitionTemplateColumnDto.newInstance(requisitionTemplate.getColumnsMap()));

    return requisitionTemplateDto;
  }
}
