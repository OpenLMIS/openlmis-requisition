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

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.javers.common.collections.Sets.asSet;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.powermock.api.mockito.PowerMockito.when;

import java.util.List;
import java.util.UUID;
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
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.service.RequestParameters;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.testutils.SupplyLineDtoDataBuilder;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.data.domain.PageRequest;

@SuppressWarnings("PMD.UnusedPrivateField")
public class RequisitionForConvertBuilderTest {

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @Mock
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  @InjectMocks
  private RequisitionForConvertBuilder requisitionForConvertBuilder =
      new RequisitionForConvertBuilder();

  private FacilityDto facility = new FacilityDtoDataBuilder().build();
  private FacilityDto facility1 = new FacilityDtoDataBuilder().build();
  private FacilityDto facility2 = new FacilityDtoDataBuilder().build();
  private UUID supervisoryNodeId1 = UUID.randomUUID();
  private UUID supervisoryNodeId2 = UUID.randomUUID();
  private ProgramDto program = new ProgramDtoDataBuilder().build();
  private Requisition requisition1 = new RequisitionDataBuilder()
      .withProgramId(program.getId())
      .withFacilityId(facility.getId())
      .withSupervisoryNodeId(supervisoryNodeId1)
      .build();
  private Requisition requisition2 = new RequisitionDataBuilder()
      .withProgramId(program.getId())
      .withFacilityId(facility.getId())
      .withSupervisoryNodeId(supervisoryNodeId2)
      .build();
  private SupplyLineDto supplyLine1 = new SupplyLineDtoDataBuilder()
      .withSupervisoryNode(supervisoryNodeId1)
      .withSupplyingFacility(facility1.getId())
      .build();
  private SupplyLineDto supplyLine2 = new SupplyLineDtoDataBuilder()
      .withSupervisoryNode(supervisoryNodeId2)
      .withSupplyingFacility(facility2.getId())
      .build();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(facilityReferenceDataService.search(
        asSet(facility.getId(), facility1.getId(), facility2.getId())))
        .thenReturn(asList(facility, facility1, facility2));
    when(programReferenceDataService.search(asSet(program.getId())))
        .thenReturn(singletonList(program));
    when(supplyLineReferenceDataService.getPage(any(RequestParameters.class)))
        .thenReturn(Pagination.getPage(asList(supplyLine1, supplyLine2),
            new PageRequest(0, Integer.MAX_VALUE), 2));
  }

  @Test
  public void shouldBuildRequisitionsWithDepotsRespectingUserAccessibleDepots() {
    //when
    List<RequisitionWithSupplyingDepotsDto> result = requisitionForConvertBuilder.buildRequisitions(
        asList(requisition1, requisition2),
        asSet(facility1.getId(), facility2.getId()),
        asList(supplyLine1, supplyLine2));

    //then
    //we have 2 requisition representations
    assertEquals(2, result.size());

    //first requisition has 2 supplying depots where user has rights
    assertEquals(facility1, result.get(0).getSupplyingDepot());

    //second requisition has 1 supplying depot where user has right
    assertEquals(facility2, result.get(1).getSupplyingDepot());

    verifyNoMoreInteractions(supplyLineReferenceDataService);
  }

  @Test
  public void shouldSearchForSupplyingFacilitiesIfNullValueWasPassed() {
    requisitionForConvertBuilder.buildRequisitions(asList(requisition1, requisition2),
        asSet(facility1.getId(), facility2.getId()), null);

    verify(supplyLineReferenceDataService).getPage(any(RequestParameters.class));
  }
}
