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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import com.google.common.collect.Sets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.service.RequestParameters;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.springframework.data.domain.PageImpl;

public class SupervisoryNodeReferenceDataServiceTest {

  private static final String PROGRAM_ID = "programId";
  private static final String FACILITY_ID = "facilityId";

  private UUID facility = UUID.randomUUID();
  private UUID program = UUID.randomUUID();
  private SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
  private SupervisoryNodeDto supervisoryNode2 = DtoGenerator.of(SupervisoryNodeDto.class);

  private SupervisoryNodeReferenceDataService service;

  @Before
  public void setUp() {
    service = spy(new SupervisoryNodeReferenceDataService());
  }

  @Test
  public void shouldReturnNullIfEmptyPage() {
    doReturn(new PageImpl<SupervisoryNodeDto>(Collections.emptyList()))
        .when(service)
        .getPage(RequestParameters.init()
            .set(PROGRAM_ID, program)
            .set(FACILITY_ID, facility));

    assertNull(service.findSupervisoryNode(program, facility));
  }

  @Test
  public void shouldReturnFirstElementIfMoreThanOneFound() {
    List<SupervisoryNodeDto> found = Arrays.asList(supervisoryNode, supervisoryNode2);
    doReturn(new PageImpl<>(found))
        .when(service)
        .getPage(RequestParameters.init()
            .set(PROGRAM_ID, program)
            .set(FACILITY_ID, facility));

    SupervisoryNodeDto foundNode = service.findSupervisoryNode(program, facility);

    assertEquals(supervisoryNode, foundNode);
    assertNotEquals(supervisoryNode2, foundNode);
  }

  @Test
  public void shouldReturnFirstElementIfOneFound() {
    doReturn(new PageImpl<>(Collections.singletonList(supervisoryNode)))
        .when(service)
        .getPage(RequestParameters.init()
            .set(PROGRAM_ID, program)
            .set(FACILITY_ID, facility));

    SupervisoryNodeDto foundNode = service.findSupervisoryNode(program, facility);

    assertEquals(supervisoryNode, foundNode);
  }

  @Test
  public void shouldFindSupervisoryNodesByIds() {
    HashSet<UUID> uuids = Sets.newHashSet(UUID.randomUUID(), UUID.randomUUID());
    List<SupervisoryNodeDto> expected = Arrays.asList(supervisoryNode, supervisoryNode2);
    doReturn(new PageImpl<>(expected))
        .when(service)
        .getPage(RequestParameters.init().set("id", uuids));

    List<SupervisoryNodeDto> supervisoryNodes = service.findByIds(uuids);

    assertEquals(expected, supervisoryNodes);
  }

}