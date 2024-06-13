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

import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.web.FacilityTypeHelper.WARD_SERVICE_TYPE_CODE;

import java.util.HashSet;
import java.util.List;
import org.javers.common.collections.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.FacilityTypeDtoDataBuilder;

@RunWith(MockitoJUnitRunner.class)
public class FacilityTypeHelperTest {

  public static final String TEST_FACILITY = "Test facility";
  public static final String TEST_CODE_TYPE = "TEST";

  @Mock
  FacilityReferenceDataService facilityReferenceDataService;

  @InjectMocks
  FacilityTypeHelper facilityTypeHelper;

  private FacilityDto facility;

  @Before
  public void setUp() {
    facility = generateInstance(TEST_CODE_TYPE);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenFacilityIsWardServiceType() {
    FacilityDto ward = generateInstance(WARD_SERVICE_TYPE_CODE);

    facilityTypeHelper.checkIfFacilityHasSupportedType(ward, TEST_FACILITY);
  }

  @Test
  public void shouldPassWhenFacilityIsOtherTypeThanWardService() {
    facilityTypeHelper.checkIfFacilityHasSupportedType(facility, TEST_FACILITY);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenOneOfTheFacilitiesIsWardServiceType() {
    FacilityDto ward = generateInstance(WARD_SERVICE_TYPE_CODE);
    List<FacilityDto> facilities = Lists.asList(ward, facility);

    when(facilityReferenceDataService.search(anySet())).thenReturn(facilities);

    facilityTypeHelper.checkIfFacilityHasSupportedType(new HashSet<>(), TEST_FACILITY);
  }

  @Test
  public void shouldPassWhenNoneOfTheFacilitiesAreWardServiceType() {
    FacilityDto anotherFacility = generateInstance("Code2");
    List<FacilityDto> facilities = Lists.asList(anotherFacility, facility);

    when(facilityReferenceDataService.search(anySet())).thenReturn(facilities);

    facilityTypeHelper.checkIfFacilityHasSupportedType(new HashSet<>(), TEST_FACILITY);
  }

  private FacilityDto generateInstance(String typeCode) {
    FacilityTypeDto facilityType = new FacilityTypeDtoDataBuilder()
        .withCode(typeCode)
        .buildAsDto();

    return new FacilityDtoDataBuilder()
        .withType(facilityType)
        .buildAsDto();
  }

}