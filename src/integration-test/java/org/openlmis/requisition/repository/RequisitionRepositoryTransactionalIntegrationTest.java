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

import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;

import java.util.UUID;
import javax.transaction.Transactional;
import org.assertj.core.util.Lists;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.orm.jpa.JpaSystemException;
import org.springframework.test.context.transaction.TestTransaction;

/**
 * Pay attention when adding tests to this class as it does NOT rollback
 * the database state after tests by default. The transactions are managed manually here,
 * so clean up anything that your tests have saved in the database in the @After-annotated method.
 */
@Transactional(Transactional.TxType.REQUIRES_NEW)
public class RequisitionRepositoryTransactionalIntegrationTest
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
    StatusChange change3 = StatusChange.newStatusChange(requisition, UUID.randomUUID());
    change3.setSupervisoryNodeId(UUID.randomUUID());
    StatusChange change4 = StatusChange.newStatusChange(requisition, UUID.randomUUID());
    change4.setSupervisoryNodeId(UUID.randomUUID());

    requisition.setStatusChanges(Lists.newArrayList(change1, change2, change3, change4));
    repository.save(requisition);

    // Trigger is fired at the end of the transaction
    TestTransaction.end();
  }

  @Test(expected = ObjectOptimisticLockingFailureException.class)
  public void shouldFailSavingRequisitionWithTheSameVersionTwice() {
    TestTransaction.flagForCommit();
    Requisition requisition = generateInstance();
    Requisition saved = repository.save(requisition);
    saved.setStatus(SUBMITTED);
    repository.save(saved);
    entityManager.flush();
    TestTransaction.end();

    // We simulate that another transaction has made a different change and commits with the same
    // version as the above one.
    TestTransaction.start();
    saved.setStatus(APPROVED);
    saved.setVersion(saved.getVersion() - 1);
    repository.save(saved);
    entityManager.flush();
    TestTransaction.end();
  }

}
