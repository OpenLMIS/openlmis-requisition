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

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class RequisitionDtoBuilder {

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  /**
   * Create a list of {@link RequisitionDto} based on passed data.
   *
   * @param requisitions a list of requisitions that will be converted into DTOs.
   * @return a list of {@link RequisitionDto}
   */
  public List<RequisitionDto> build(Collection<Requisition> requisitions) {
    return requisitions.stream().map(this::build).collect(Collectors.toList());
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public RequisitionDto build(Requisition requisition) {
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());

    return build(requisition, facility, program);
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public RequisitionDto build(Requisition requisition, FacilityDto facility, ProgramDto program) {
    if (null == requisition) {
      return null;
    }

    RequisitionDto requisitionDto = new RequisitionDto();

    requisition.export(requisitionDto);

    RequisitionTemplateDto template = RequisitionTemplateDto.newInstance(requisition.getTemplate());
    if (template != null) {
      template.setProgramId(null);
    }
    requisitionDto.setTemplate(template);
    if (facility != null) {
      facility.setSupportedPrograms(null);
    }
    requisitionDto.setFacility(facility);
    requisitionDto.setProgram(program);
    requisitionDto.setProcessingPeriod(periodService.getPeriod(
        requisition.getProcessingPeriodId()));

    List<RequisitionLineItemDto> requisitionLineItemDtoList =
        requisitionExportHelper.exportToDtos(requisition.getRequisitionLineItems());
    requisitionDto.setRequisitionLineItems(requisitionLineItemDtoList);

    if (null != requisition.getAvailableNonFullSupplyProducts()) {
      requisitionDto.setAvailableNonFullSupplyProducts(
          requisition.getAvailableNonFullSupplyProducts()
              .stream()
              .filter(Objects::nonNull)
              .map(orderableReferenceDataService::findOne)
              .collect(Collectors.toSet())
      );
    }

    return requisitionDto;
  }

}
