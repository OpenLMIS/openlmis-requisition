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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.powermock.api.mockito.PowerMockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.springframework.data.domain.PageImpl;

@SuppressWarnings("PMD.UnusedPrivateField")
public class RequisitionForConvertBuilderTest {

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @InjectMocks
  private RequisitionForConvertBuilder requisitionForConvertBuilder =
      new RequisitionForConvertBuilder();

  private FacilityDto facility = mockFacility();
  private ProgramDto program = new ProgramDtoDataBuilder().build();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(facilityReferenceDataService.findOne(facility.getId()))
        .thenReturn(facility);
    when(programReferenceDataService.findOne(program.getId()))
        .thenReturn(program);
  }

  @Test
  public void shouldBuildRequisitionsWithDepotsRespectingUserAccessibleDepots() {
    //given
    FacilityDto facility1 = mockFacility();
    FacilityDto facility2 = mockFacility();
    FacilityDto facility3 = mockFacility();

    List<UUID> userManagedDepots = Lists.newArrayList(facility1.getId(), facility2.getId());
    when(facilityReferenceDataService.searchSupplyingDepots(any(), any()))
        .thenReturn(Lists.newArrayList(facility1, facility2, facility3))
        .thenReturn(Lists.newArrayList(facility1, facility3));

    Requisition requisition = mockRequisition();
    Requisition requisition2 = mockRequisition();
    List requisitionsList = Lists.newArrayList(requisition, requisition2);

    //when
    List<RequisitionWithSupplyingDepotsDto> result = requisitionForConvertBuilder
        .buildRequisitions(new PageImpl<Requisition>(requisitionsList), userManagedDepots);

    //then
    //we have 2 requisition representations
    assertEquals(2, result.size());

    //first requisition has 2 supplying depots where user has rights
    assertEquals(2, result.get(0).getSupplyingDepots().size());

    //second requisition has 1 supplying depot where user has right
    assertEquals(1, result.get(1).getSupplyingDepots().size());
  }

  @Test
  public void shouldGetSupplyingFacilitiesFromCache() {
    Requisition requisition1 = mockRequisition();
    Requisition requisition2 = mockRequisition(
        requisition1.getProgramId(), requisition1.getSupervisoryNodeId());
    Requisition requisition3 = mockRequisition(
        requisition1.getProgramId(), requisition1.getSupervisoryNodeId());

    when(programReferenceDataService.findOne(requisition1.getProgramId()))
        .thenReturn(program);

    List requisitionsList = Lists.newArrayList(requisition1, requisition2, requisition3);

    requisitionForConvertBuilder.buildRequisitions(
        new PageImpl<Requisition>(requisitionsList), new ArrayList<>());

    // Should hit ref data once and then use cache
    verify(facilityReferenceDataService, times(1))
        .searchSupplyingDepots(any(UUID.class), any(UUID.class));
    verifyNoMoreInteractions(facilityReferenceDataService);
  }

  @Test
  public void sholdQueryRefDataForSupplyingFacilitiesWhenNoCacheHits() {
    Requisition requisition1 = mockRequisition();
    Requisition requisition2 = mockRequisition();
    Requisition requisition3 = mockRequisition();

    List requisitionsList = Lists.newArrayList(requisition1, requisition2, requisition3);

    requisitionForConvertBuilder.buildRequisitions(
        new PageImpl<Requisition>(requisitionsList), new ArrayList<>());

    // Should hit ref data three times - for each requisition
    verify(facilityReferenceDataService, times(3))
        .searchSupplyingDepots(any(UUID.class), any(UUID.class));
    verifyNoMoreInteractions(facilityReferenceDataService);
  }

  private FacilityDto mockFacility() {
    return new FacilityDtoDataBuilder().build();
  }

  private Requisition mockRequisition() {
    return mockRequisition(program.getId(), UUID.randomUUID());
  }

  private Requisition mockRequisition(UUID programId, UUID supervisoryNodeId) {
    return new RequisitionDataBuilder()
        .withProgramId(programId)
        .withFacilityId(facility.getId())
        .withSupervisoryNodeId(supervisoryNodeId)
        .build();
  }
}
