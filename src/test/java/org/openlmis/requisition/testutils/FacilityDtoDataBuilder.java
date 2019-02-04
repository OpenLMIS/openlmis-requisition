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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.FacilityOperatorDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.SupportedProgramDto;

public class FacilityDtoDataBuilder {

  private static int instanceNumber = 0;

  private UUID id;
  private String code;
  private String name;
  private Boolean active;
  private GeographicZoneDto geographicZone;
  private FacilityTypeDto type;
  private String description;
  private LocalDate goLiveDate;
  private LocalDate goDownDate;
  private String comment;
  private Boolean enabled;
  private Boolean openLmisAccessible;
  private List<SupportedProgramDto> supportedPrograms;
  private FacilityOperatorDto operator;

  /**
   * Creates builder for creating new instance of {@link FacilityDto}.
   */
  public FacilityDtoDataBuilder() {
    id = UUID.randomUUID();
    code = "F" + instanceNumber;
    name = "Facility " + instanceNumber;
    active = true;
    geographicZone = new GeographicZoneDtoDataBuilder().build();
    type = new FacilityTypeDto();
    description = "facility desc " + instanceNumber;
    goLiveDate = LocalDate.now();
    goDownDate = null;
    comment = "some facility comment " + instanceNumber;
    enabled = true;
    openLmisAccessible = true;
    supportedPrograms = new ArrayList<>();
    operator = new FacilityOperatorDto();
  }

  /**
   * Creates new instance of {@link FacilityDto} with properties.
   * @return created facility.
   */
  public FacilityDto build() {
    FacilityDto facility = new FacilityDto();
    facility.setId(id);
    facility.setCode(code);
    facility.setName(name);
    facility.setActive(active);
    facility.setGeographicZone(geographicZone);
    facility.setType(type);
    facility.setDescription(description);
    facility.setGoLiveDate(goLiveDate);
    facility.setGoDownDate(goDownDate);
    facility.setComment(comment);
    facility.setEnabled(enabled);
    facility.setOpenLmisAccessible(openLmisAccessible);
    facility.setSupportedPrograms(supportedPrograms);
    facility.setOperator(operator);

    return facility;
  }
}
