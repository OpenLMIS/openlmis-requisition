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

package org.openlmis.requisition;

import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.BDDMockito.given;
import static org.openlmis.requisition.utils.Pagination.DEFAULT_PAGE_NUMBER;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import org.javers.core.Javers;
import org.javers.core.metamodel.object.CdoSnapshot;
import org.javers.repository.jql.QueryBuilder;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.CodeDto;
import org.openlmis.requisition.web.BaseWebIntegrationTest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ActiveProfiles("test")
public class AuditLogInitializerIntegrationTest extends BaseWebIntegrationTest {

  private List<CdoSnapshot> snapshots;

  @Autowired
  private Javers javers;

  @MockBean
  private AuditLogInitializer auditLogInitializer;

  @Test
  public void shouldReturnEmptyPageAfterRequisitionIsCreated() throws IOException {
    // given
    generateRequisition();
    final Pageable pageable = new PageRequest(DEFAULT_PAGE_NUMBER, 2000);
    final Page<Requisition> emptyPage = new PageImpl<>(Collections.emptyList());

    //when
    auditLogInitializer.createSnapshots(requisitionRepository);

    //then
    snapshots = javers.findSnapshots(QueryBuilder.byClass(CodeDto.class).build());

    assertNotNull(snapshots);
    given(requisitionRepository.findAllWithoutSnapshots(pageable)).willReturn(emptyPage);
  }

  @Test
  public void shouldReturnNotEmptyPage() throws IOException {
    final Pageable pageable = new PageRequest(DEFAULT_PAGE_NUMBER, 2000);
    Page<Requisition> emptyPage = new PageImpl<>(Collections.emptyList());

    assertNotEquals(requisitionRepository.findAllWithoutSnapshots(pageable), emptyPage);
  }
}
