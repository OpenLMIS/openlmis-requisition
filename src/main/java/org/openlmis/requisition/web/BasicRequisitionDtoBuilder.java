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
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class BasicRequisitionDtoBuilder {

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  /**
   * Create a list of {@link BasicRequisitionDto} based on passed data.
   *
   * @param requisitions a list of requisitions that will be converted into basic DTOs.
   * @return a list of {@link BasicRequisitionDto}
   */
  public List<BasicRequisitionDto> build(Collection<Requisition> requisitions) {
    return requisitions.stream().map(this::build).collect(Collectors.toList());
  }

  /**
   * Create a new instance of BasicRequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link BasicRequisitionDto} (can be {@code null})
   * @return new instance of {@link BasicRequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public BasicRequisitionDto build(Requisition requisition) {
    if (null == requisition) {
      return null;
    }

    BasicRequisitionDto requisitionDto = new BasicRequisitionDto();

    requisition.export(requisitionDto);

    requisitionDto.setFacility(facilityReferenceDataService.findOne(requisition.getFacilityId()));
    requisitionDto.setProgram(programReferenceDataService.findOne(requisition.getProgramId()));

    requisitionDto.setProcessingPeriod(periodService.getPeriod(
        requisition.getProcessingPeriodId()
    ));

    return requisitionDto;
  }

}
