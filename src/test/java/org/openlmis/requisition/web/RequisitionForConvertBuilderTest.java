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

import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.powermock.api.mockito.PowerMockito.when;

@SuppressWarnings("PMD.UnusedPrivateField")
public class RequisitionForConvertBuilderTest {

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @InjectMocks
  private RequisitionForConvertBuilder requisitionForConvertBuilder =
      new RequisitionForConvertBuilder();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void shouldBuildRequisitionsWithDepotsRespectingUserAccessibleDepots() {
    //given
    FacilityDto facility1 = mockSupplyingDepot();
    FacilityDto facility2 = mockSupplyingDepot();
    FacilityDto facility3 = mockSupplyingDepot();

    List<UUID> userManagedDepots = Lists.newArrayList(facility1.getId(), facility2.getId());
    when(facilityReferenceDataService.searchSupplyingDepots(any(), any()))
        .thenReturn(Lists.newArrayList(facility1, facility2, facility3))
        .thenReturn(Lists.newArrayList(facility1, facility3));

    Requisition requisition = mockRequisition();
    Requisition requisition2 = mockRequisition();

    //when
    List<RequisitionWithSupplyingDepotsDto> result = requisitionForConvertBuilder
        .buildRequisitions(Lists.newArrayList(requisition, requisition2), userManagedDepots,
            Collections.emptyList(), Collections.emptyList());

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

    requisitionForConvertBuilder.buildRequisitions(
        Lists.newArrayList(requisition1, requisition2, requisition3), new ArrayList<>(),
        Collections.emptyList(), Collections.emptyList());

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

    requisitionForConvertBuilder.buildRequisitions(
        Lists.newArrayList(requisition1, requisition2, requisition3), new ArrayList<>(),
        Collections.emptyList(), Collections.emptyList());

    // Should hit ref data three times - for each requisition
    verify(facilityReferenceDataService, times(3))
        .searchSupplyingDepots(any(UUID.class), any(UUID.class));
    verifyNoMoreInteractions(facilityReferenceDataService);
  }

  private FacilityDto mockSupplyingDepot() {
    FacilityDto facilityDto = new FacilityDto();
    facilityDto.setId(UUID.randomUUID());
    return facilityDto;
  }

  private Requisition mockRequisition() {
    return mockRequisition(UUID.randomUUID(), UUID.randomUUID());
  }

  private Requisition mockRequisition(UUID program, UUID supervisoryNode) {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setProgramId(program);
    requisition.setSupervisoryNodeId(supervisoryNode);
    return requisition;
  }

}
