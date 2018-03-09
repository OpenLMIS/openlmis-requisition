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

package org.openlmis.requisition.repository;

import org.assertj.core.util.Lists;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.springframework.orm.jpa.JpaSystemException;
import org.springframework.test.context.transaction.TestTransaction;

import java.util.UUID;

import javax.transaction.Transactional;

@Transactional(Transactional.TxType.REQUIRES_NEW)
public class RequisitionRepositoryStatusChangesIntegrationTest
    extends BaseRequisitionRepositoryIntegrationTest {

  @Before
  public void setUp() {
    testTemplate = templateRepository.save(new RequisitionTemplateDataBuilder().build());
  }

  @After
  public void cleanUp() {
    repository.deleteAll();
    templateRepository.deleteAll();
  }

  @Test(expected = JpaSystemException.class)
  public void shouldNotAllowDuplicateStatusChanges() {
    TestTransaction.flagForCommit();
    Requisition requisition = generateInstance();
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setStatusChanges(Lists.newArrayList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID()),
        StatusChange.newStatusChange(requisition, UUID.randomUUID())
        )
    );

    repository.save(requisition);

    // Trigger is fired at the end of the transaction
    TestTransaction.end();
  }

  @Test
  public void shouldAllowStatusChangesWithDifferentSupervisoryNodes() {
    TestTransaction.flagForCommit();
    Requisition requisition = generateInstance();
    requisition.setStatus(RequisitionStatus.IN_APPROVAL);

    StatusChange change1 = StatusChange.newStatusChange(requisition, UUID.randomUUID());
    change1.setSupervisoryNodeId(UUID.randomUUID());

    StatusChange change2 = StatusChange.newStatusChange(requisition, UUID.randomUUID());
    change2.setSupervisoryNodeId(UUID.randomUUID());

    requisition.setStatusChanges(Lists.newArrayList(change1, change2));
    repository.save(requisition);

    // Trigger is fired at the end of the transaction
    TestTransaction.end();
  }

}
