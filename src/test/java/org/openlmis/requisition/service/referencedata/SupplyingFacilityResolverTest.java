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
  public void shouldResolveDistinctSupplyingFacilities() {
    FacilityDto facilityA = new FacilityDtoDataBuilder().buildAsDto();
    FacilityDto facilityB = new FacilityDtoDataBuilder().buildAsDto();
    when(supplyLineReferenceDataService.search(programId, supervisoryNodeId))
        .thenReturn(Arrays.asList(
            new SupplyLineDtoDataBuilder().withSupplyingFacility(facilityA).buildAsDto(),
            new SupplyLineDtoDataBuilder().withSupplyingFacility(facilityA).buildAsDto(),
            new SupplyLineDtoDataBuilder().withSupplyingFacility(facilityB).buildAsDto()));

    List<FacilityDto> result = resolver.resolve(requisition);

    assertThat(result, contains(facilityA, facilityB));
  }

  @Test
  public void shouldReturnEmptyWhenNoSupplyLines() {
    when(supplyLineReferenceDataService.search(programId, supervisoryNodeId))
        .thenReturn(Collections.emptyList());

    assertThat(resolver.resolve(requisition), hasSize(0));
  }

  @Test
  public void shouldReturnEmptyAndSkipSearchWhenNoSupervisoryNode() {
    when(requisition.getSupervisoryNodeId()).thenReturn(null);

    assertThat(resolver.resolve(requisition), hasSize(0));
    verify(supplyLineReferenceDataService, never()).search(any(UUID.class), any(UUID.class));
  }
}
