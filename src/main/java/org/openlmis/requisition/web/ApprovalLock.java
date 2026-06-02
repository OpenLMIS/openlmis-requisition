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

import java.util.UUID;
import java.util.concurrent.ScheduledFuture;

/** Holds an acquired approval lock: its owner token and the task that keeps its TTL alive. */
final class ApprovalLock {

  private final UUID requisitionId;
  private final String token;
  private final ScheduledFuture<?> renewalTask;

  ApprovalLock(UUID requisitionId, String token, ScheduledFuture<?> renewalTask) {
    this.requisitionId = requisitionId;
    this.token = token;
    this.renewalTask = renewalTask;
  }

  UUID getRequisitionId() {
    return requisitionId;
  }

  String getToken() {
    return token;
  }

  ScheduledFuture<?> getRenewalTask() {
    return renewalTask;
  }
}
