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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.UUID;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.repository.custom.ProcessedRequestsRedisRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class ProcessedRequestsRedisRepositoryIntegrationTest {

  private UUID idempotencyKey1 = UUID.randomUUID();
  private UUID idempotencyKey2 = UUID.randomUUID();

  private UUID resource1 = UUID.randomUUID();
  private UUID resource2 = UUID.randomUUID();

  @Autowired
  private ProcessedRequestsRedisRepository redisRepository;
  
  @Test
  public void shouldSaveAndFindIdempotencyKeysWithoutLocation() {
    redisRepository.addOrUpdate(idempotencyKey1, null);
    redisRepository.addOrUpdate(idempotencyKey2, null);

    assertTrue(redisRepository.exists(idempotencyKey1));
    assertTrue(redisRepository.exists(idempotencyKey2));
    assertFalse(redisRepository.exists(UUID.randomUUID()));
  }

  @Test
  public void shouldSaveAndFindIdempotencyKeysWithLocation() {
    redisRepository.addOrUpdate(idempotencyKey1, resource1);
    redisRepository.addOrUpdate(idempotencyKey2, resource2);

    assertEquals(resource1, redisRepository.findByIdempotencyKey(idempotencyKey1));
    assertEquals(resource2, redisRepository.findByIdempotencyKey(idempotencyKey2));
    assertNull(redisRepository.findByIdempotencyKey(UUID.randomUUID()));
  }

  @Test
  public void shouldUpdateLocationForIdempotencyKey() {
    redisRepository.addOrUpdate(idempotencyKey1, resource1);
    assertEquals(resource1, redisRepository.findByIdempotencyKey(idempotencyKey1));

    redisRepository.addOrUpdate(idempotencyKey1, resource2);
    assertEquals(resource2, redisRepository.findByIdempotencyKey(idempotencyKey1));
  }

  @Test
  public void shouldLockRequisitionForApprovalOnlyOnceUntilUnlocked() {
    UUID requisitionId = UUID.randomUUID();
    String token = null;
    try {
      token = redisRepository.lockRequisitionForApproval(requisitionId);
      assertNotNull(token);
      assertNull(redisRepository.lockRequisitionForApproval(requisitionId));

      assertTrue(redisRepository.unlockRequisitionForApproval(requisitionId, token));
      token = redisRepository.lockRequisitionForApproval(requisitionId);
      assertNotNull(token);
    } finally {
      if (token != null) {
        redisRepository.unlockRequisitionForApproval(requisitionId, token);
      }
    }
  }

  @Test
  public void shouldNotUnlockApprovalWhenTokenDoesNotMatch() {
    // OLMIS-8206: an approval whose lock expired and was taken over must not release the new lock.
    UUID requisitionId = UUID.randomUUID();
    String token = redisRepository.lockRequisitionForApproval(requisitionId);
    try {
      assertNotNull(token);
      assertFalse(redisRepository.unlockRequisitionForApproval(requisitionId, "stale-token"));
      // the real lock is still held, so it cannot be acquired again
      assertNull(redisRepository.lockRequisitionForApproval(requisitionId));
    } finally {
      redisRepository.unlockRequisitionForApproval(requisitionId, token);
    }
  }

  @Test
  public void shouldRenewApprovalLockOnlyForMatchingToken() {
    // OLMIS-8206: only the current holder may extend the lock's expiry.
    UUID requisitionId = UUID.randomUUID();
    String token = redisRepository.lockRequisitionForApproval(requisitionId);
    try {
      assertNotNull(token);
      assertTrue(redisRepository.renewApprovalLock(requisitionId, token));
      assertFalse(redisRepository.renewApprovalLock(requisitionId, "stale-token"));
    } finally {
      redisRepository.unlockRequisitionForApproval(requisitionId, token);
    }
  }
}
