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

package org.openlmis.requisition.web;

import static org.openlmis.requisition.web.ResourceNames.FACILITY_TYPES;
import static org.openlmis.requisition.web.ResourceNames.PROGRAMS;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ObjectReferenceDto;
import org.openlmis.requisition.dto.RequisitionTemplateColumnDto;
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Component
public class RequisitionTemplateDtoBuilder {

  @Value("${service.url}")
  private String serviceUrl;

  /**
   * Create new list of RequisitionTemplateDto based on given list of {@link RequisitionTemplate}
   *
   * @param templates list of {@link RequisitionTemplate}
   * @return new list of RequisitionTemplateDto.
   */
  public Iterable<RequisitionTemplateDto> newInstance(Iterable<RequisitionTemplate> templates) {
    return StreamSupport
        .stream(templates.spliterator(), false)
        .map(this::newInstance)
        .collect(Collectors.toList());
  }

  /**
   * Create new instance of RequisitionTemplateDto based on given {@link RequisitionTemplate}
   *
   * @param template instance of Template
   * @return new instance of RequisitionTemplateDto.
   */
  public RequisitionTemplateDto newInstance(RequisitionTemplate template) {
    if (null == template) {
      return null;
    }

    RequisitionTemplateDto dto = new RequisitionTemplateDto();
    template.export(dto);
    dto.setColumnsMap(RequisitionTemplateColumnDto.newInstance(template.getColumnsMap()));
    dto.setProgram(new ObjectReferenceDto(template.getProgramId(), serviceUrl, PROGRAMS));
    dto.setFacilityTypes(template
        .getFacilityTypeIds()
        .stream()
        .map(elem -> new ObjectReferenceDto(elem, serviceUrl, FACILITY_TYPES))
        .collect(Collectors.toSet()));
    dto.setName(template.getName());

    return dto;
  }

}
