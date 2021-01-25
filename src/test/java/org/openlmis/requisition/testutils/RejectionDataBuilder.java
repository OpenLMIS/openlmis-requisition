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

package org.openlmis.requisition.testutils;

import java.time.ZonedDateTime;
import java.util.UUID;

import org.openlmis.requisition.domain.Rejection;
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.domain.requisition.StatusChange;

public class RejectionDataBuilder {
  private UUID id;
  private ZonedDateTime createdDate;
  private RejectionReason rejectionReason;
  private StatusChange statusChange;

  /**
   * Builds instance of {@link RejectionReasonDataBuilder} with sample data.
   */
  public RejectionDataBuilder() {
    id = UUID.randomUUID();
    createdDate = ZonedDateTime.now();
    rejectionReason = new RejectionReasonDataBuilder().build();
    statusChange = new StatusChangeDataBuilder().build();
    createdDate = ZonedDateTime.now();
  }

  /**
   * Builds instance of {@link org.openlmis.requisition.domain.RejectionReason}.
   */
  public Rejection build() {
    Rejection rejection = Rejection.newRejection(rejectionReason, statusChange);
    rejection.setCreatedDate(createdDate);
    rejection.setId(id);
    return rejection;
  }

  /**
   * Builds instance of {@link Rejection} without id.
   */
  public Rejection buildAsNew() {
    return this.withoutId().build();
  }

  public RejectionDataBuilder withoutId() {
    this.id = null;
    return this;
  }

  public RejectionDataBuilder withName(RejectionReason rejectionReason) {
    this.rejectionReason = rejectionReason;
    return this;
  }

  public RejectionDataBuilder withCode(StatusChange statusChange) {
    this.statusChange = statusChange;
    return this;
  }
}
