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

package org.openlmis.utils;

import static org.mockito.Mockito.when;
import static org.openlmis.utils.FacilitySupportsProgramHelper.REQUISITION_TIME_ZONE_ID;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;

import java.time.LocalDate;
import java.util.Collections;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class FacilitySupportsProgramHelperTest {

  private static final LocalDate SUPPORT_START_DATE = LocalDate.of(2011,12,12);

  @Mock
  FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  ConfigurationSettingService configurationSettingService;

  @InjectMocks
  FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  private FacilityDto facilityDto;
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();

  @Before
  public void setUp() {
    facilityDto = new FacilityDto();

    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(facilityDto);
    when(configurationSettingService.getStringValue(REQUISITION_TIME_ZONE_ID)).thenReturn("UTC");
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenNoSupportedPrograms() {
    facilityDto.setSupportedPrograms(Collections.emptyList());

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenProgramIsNotSupported() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(UUID.randomUUID());
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test
  public void shouldPassWhenProgramIsSupported() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSupportIsNotActive() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(false);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenProgramIsNotActive() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(false);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenStartDateIsAfterCurrentDate() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(false);
    supportedProgramDto.setSupportStartDate(LocalDate.now().plusDays(1));

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test
  public void shouldPassWhenProgramStartDateIsNull() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(null);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

}
