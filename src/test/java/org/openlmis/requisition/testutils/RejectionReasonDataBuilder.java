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

import java.util.UUID;
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.domain.RejectionReasonCategory;

public class RejectionReasonDataBuilder {

  private static int instanceNumber = 0;

  private UUID id;
  private String name;
  private String code;
  private RejectionReasonCategory rejectionReasonCategory;

  /**
   * Builds instance of {@link RejectionReasonDataBuilder} with sample data.
   */
  public RejectionReasonDataBuilder() {
    instanceNumber++;

    id = UUID.randomUUID();
    name = "Rejection" + instanceNumber;
    code = "RJ" + instanceNumber;

  }

  /**
   * Builds instance of {@link org.openlmis.requisition.domain.RejectionReason}.
   */
  public RejectionReason build() {
    RejectionReason rejectionReason = RejectionReason.newRejectionReason(name, code,
            rejectionReasonCategory);
    rejectionReason.setId(id);
    return rejectionReason;
  }

  /**
   * Builds instance of {@link RejectionReason} without id.
   */
  public RejectionReason buildAsNew() {
    return this.withoutId().build();
  }

  public RejectionReasonDataBuilder withoutId() {
    this.id = null;
    return this;
  }

  public RejectionReasonDataBuilder withName(String name) {
    this.name = name;
    return this;
  }

  public RejectionReasonDataBuilder withCode(String code) {
    this.code = code;
    return this;
  }

  public RejectionReasonDataBuilder withCategory(RejectionReasonCategory rejectionReasonCategory) {
    this.rejectionReasonCategory = rejectionReasonCategory;
    return this;
  }
}
