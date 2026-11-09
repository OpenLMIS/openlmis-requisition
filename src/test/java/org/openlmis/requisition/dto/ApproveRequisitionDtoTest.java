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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collections;
import java.util.UUID;
import org.junit.Test;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;

public class ApproveRequisitionDtoTest {

  private static final String CODE = "REQ-HC01-202611-0001";

  @Test
  public void shouldCopyCodeFromRequisitionDto() {
    RequisitionDto requisition = new RequisitionDto();
    requisition.setId(UUID.randomUUID());
    requisition.setCode(CODE);
    requisition.setFacility(new FacilityDto());
    requisition.setProcessingPeriod(new ProcessingPeriodDto());
    requisition.setRequisitionLineItems(Collections.emptyList());

    ApproveRequisitionDto dto = new ApproveRequisitionDto(requisition, UUID.randomUUID(),
        Collections.emptyMap());

    assertThat(dto.getCode()).isEqualTo(CODE);
  }

  @Test
  public void shouldCopyCodeFromRequisition() {
    Requisition requisition = new RequisitionDataBuilder().withCode(CODE).build();

    ApproveRequisitionDto dto = new ApproveRequisitionDto(requisition);

    assertThat(dto.getCode()).isEqualTo(CODE);
    assertThat(dto.getId()).isEqualTo(requisition.getId());
  }
}
