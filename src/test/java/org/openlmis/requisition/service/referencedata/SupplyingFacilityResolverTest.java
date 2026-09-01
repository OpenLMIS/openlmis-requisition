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

package org.openlmis.requisition.service.referencedata;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anySet;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.SupplyLineDtoDataBuilder;

@RunWith(MockitoJUnitRunner.class)
public class SupplyingFacilityResolverTest {

  @Mock
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @InjectMocks
  private SupplyingFacilityResolver resolver;

  private final UUID programId = UUID.randomUUID();
  private final UUID supervisoryNodeId = UUID.randomUUID();

  private Requisition requisition;

  @Before
  public void setUp() {
    requisition = mock(Requisition.class);
    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
  }

  @Test
  public void shouldResolveDistinctSupplyingFacilitiesWithDetails() {
    UUID idA = UUID.randomUUID();
    UUID idB = UUID.randomUUID();
    // SupplyLines only reference the facility (id, no code or name).
    FacilityDto referenceA = new FacilityDtoDataBuilder().withId(idA)
        .withCode(null).withName(null).buildAsDto();
    FacilityDto referenceB = new FacilityDtoDataBuilder().withId(idB)
        .withCode(null).withName(null).buildAsDto();
    FacilityDto detailedA = new FacilityDtoDataBuilder().withId(idA).buildAsDto();
    FacilityDto detailedB = new FacilityDtoDataBuilder().withId(idB).buildAsDto();

    when(supplyLineReferenceDataService.search(programId, supervisoryNodeId))
        .thenReturn(Arrays.asList(
            new SupplyLineDtoDataBuilder().withSupplyingFacility(referenceA).buildAsDto(),
            new SupplyLineDtoDataBuilder().withSupplyingFacility(referenceA).buildAsDto(),
            new SupplyLineDtoDataBuilder().withSupplyingFacility(referenceB).buildAsDto()));
    when(facilityReferenceDataService.search(anySet()))
        .thenReturn(Arrays.asList(detailedA, detailedB));

    List<FacilityDto> result = resolver.resolve(requisition);

    assertThat(result, contains(detailedA, detailedB));
  }

  @Test
  public void shouldFallBackToSupplyLineFacilityWhenLookupMisses() {
    UUID idA = UUID.randomUUID();
    UUID idB = UUID.randomUUID();
    FacilityDto referenceA = new FacilityDtoDataBuilder().withId(idA).buildAsDto();
    FacilityDto referenceB = new FacilityDtoDataBuilder().withId(idB).buildAsDto();
    FacilityDto detailedA = new FacilityDtoDataBuilder().withId(idA).buildAsDto();

    when(supplyLineReferenceDataService.search(programId, supervisoryNodeId))
        .thenReturn(Arrays.asList(
            new SupplyLineDtoDataBuilder().withSupplyingFacility(referenceA).buildAsDto(),
            new SupplyLineDtoDataBuilder().withSupplyingFacility(referenceB).buildAsDto()));
    when(facilityReferenceDataService.search(anySet()))
        .thenReturn(Collections.singletonList(detailedA));

    List<FacilityDto> result = resolver.resolve(requisition);

    assertThat(result, contains(detailedA, referenceB));
  }

  @Test
  public void shouldKeepSupplyLineFacilitiesWhenLookupFails() {
    FacilityDto referenceA = new FacilityDtoDataBuilder().buildAsDto();

    when(supplyLineReferenceDataService.search(programId, supervisoryNodeId))
        .thenReturn(Collections.singletonList(
            new SupplyLineDtoDataBuilder().withSupplyingFacility(referenceA).buildAsDto()));
    when(facilityReferenceDataService.search(anySet()))
        .thenThrow(new IllegalStateException("reference data down"));

    List<FacilityDto> result = resolver.resolve(requisition);

    assertThat(result, contains(referenceA));
  }

  @Test
  public void shouldReturnEmptyAndSkipLookupWhenNoSupplyLines() {
    when(supplyLineReferenceDataService.search(programId, supervisoryNodeId))
        .thenReturn(Collections.emptyList());

    assertThat(resolver.resolve(requisition), hasSize(0));
    verify(facilityReferenceDataService, never()).search(anySet());
  }

  @Test
  public void shouldReturnEmptyAndSkipSearchWhenNoSupervisoryNode() {
    when(requisition.getSupervisoryNodeId()).thenReturn(null);

    assertThat(resolver.resolve(requisition), hasSize(0));
    verify(supplyLineReferenceDataService, never()).search(any(UUID.class), any(UUID.class));
    verify(facilityReferenceDataService, never()).search(anySet());
  }
}
