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

package org.openlmis.requisition.repository.custom.impl;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import javax.annotation.PostConstruct;
import org.apache.commons.lang3.StringUtils;
import org.openlmis.requisition.repository.custom.ProcessedRequestsRedisRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Repository;

@Repository
public class ProcessedRequestsRedisRepositoryImpl implements ProcessedRequestsRedisRepository {

  private static final String HASH_KEY = "PROCESSED_REQUESTS";
  private static final String APPROVE_LOCK_PREFIX = "REQUISITION_APPROVE_LOCK:";

  // Release/renew only when the stored value still equals our token, so an approval that outlived
  // its TTL cannot delete or extend a lock that a later approval has already taken over.
  private static final RedisScript<Long> UNLOCK_SCRIPT = new DefaultRedisScript<>(
      "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('del', KEYS[1]) "
          + "else return 0 end", Long.class);
  private static final RedisScript<Long> RENEW_SCRIPT = new DefaultRedisScript<>(
      "if redis.call('get', KEYS[1]) == ARGV[1] "
          + "then return redis.call('pexpire', KEYS[1], ARGV[2]) else return 0 end", Long.class);

  private static final Logger LOGGER =
      LoggerFactory.getLogger(ProcessedRequestsRedisRepositoryImpl.class);

  @Value("${approval.lock.timeoutMinutes}")
  private long approvalLockTimeoutMinutes;

  private final RedisTemplate<String, String> redisTemplate;
  private final ScheduledExecutorService approvalLockRenewalScheduler;
  private final Map<String, ScheduledFuture<?>> approvalLockRenewals = new ConcurrentHashMap<>();
  private HashOperations hashOperations;
  private StringRedisTemplate lockRedisTemplate;

  @Autowired
  public ProcessedRequestsRedisRepositoryImpl(RedisTemplate<String, String> redisTemplate,
      ScheduledExecutorService approvalLockRenewalScheduler) {
    this.redisTemplate = redisTemplate;
    this.approvalLockRenewalScheduler = approvalLockRenewalScheduler;
  }

  @PostConstruct
  private void init() {
    hashOperations = redisTemplate.opsForHash();
    // Plain-string template for the lock keys so the Lua scripts compare tokens and pass the
    // expiry as readable values; the shared template uses JDK serialization for idempotency data.
    lockRedisTemplate = new StringRedisTemplate(redisTemplate.getConnectionFactory());
    lockRedisTemplate.afterPropertiesSet();
  }

  @Override
  public boolean exists(UUID idempotencyKey) {
    String key = idempotencyKey.toString();
    return hashOperations.hasKey(key, HASH_KEY);
  }

  @Override
  public UUID findByIdempotencyKey(UUID idempotencyKey) {
    String resource = (String) hashOperations.get(idempotencyKey.toString(), HASH_KEY);
    return StringUtils.isBlank(resource) ? null : UUID.fromString(resource);
  }

  @Override
  public void addOrUpdate(UUID key, UUID resourceId) {
    hashOperations.put(key.toString(), HASH_KEY, resourceId == null
        ? StringUtils.EMPTY : resourceId.toString());
    redisTemplate.expire(key.toString(), 24, TimeUnit.HOURS);
  }

  @Override
  public String lockRequisitionForApproval(UUID requisitionId) {
    // Atomic acquire-if-absent with a TTL crash safety-net. The value is a per-acquisition token
    // so release and renewal can tell whether the lock sitting here is still ours.
    String token = UUID.randomUUID().toString();
    Boolean acquired = lockRedisTemplate.opsForValue().setIfAbsent(
        APPROVE_LOCK_PREFIX + requisitionId, token,
        approvalLockTimeoutMinutes, TimeUnit.MINUTES);
    if (!Boolean.TRUE.equals(acquired)) {
      return null;
    }
    // Keep the lock alive while the approval runs; if this node dies the renewal stops and the
    // TTL frees the lock on its own.
    long intervalMillis = TimeUnit.MINUTES.toMillis(approvalLockTimeoutMinutes) / 3;
    approvalLockRenewals.put(token, approvalLockRenewalScheduler.scheduleWithFixedDelay(
        () -> renewQuietly(requisitionId, token), intervalMillis, intervalMillis,
        TimeUnit.MILLISECONDS));
    return token;
  }

  @Override
  public boolean unlockRequisitionForApproval(UUID requisitionId, String token) {
    ScheduledFuture<?> renewal = approvalLockRenewals.remove(token);
    if (renewal != null) {
      renewal.cancel(false);
    }
    Long released = lockRedisTemplate.execute(UNLOCK_SCRIPT,
        Collections.singletonList(APPROVE_LOCK_PREFIX + requisitionId), token);
    return Long.valueOf(1).equals(released);
  }

  @Override
  public boolean renewApprovalLock(UUID requisitionId, String token) {
    long leaseMillis = TimeUnit.MINUTES.toMillis(approvalLockTimeoutMinutes);
    Long renewed = lockRedisTemplate.execute(RENEW_SCRIPT,
        Collections.singletonList(APPROVE_LOCK_PREFIX + requisitionId),
        token, Long.toString(leaseMillis));
    return Long.valueOf(1).equals(renewed);
  }

  private void renewQuietly(UUID requisitionId, String token) {
    try {
      renewApprovalLock(requisitionId, token);
    } catch (RuntimeException ex) {
      // Swallow so a transient Redis error does not cancel the recurring renewal task.
      LOGGER.warn("Could not renew approval lock for requisition {}", requisitionId, ex);
    }
  }
}
