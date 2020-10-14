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
import org.openlmis.requisition.domain.RejectionReasonCategory;

public class RejectionReasonCategoryDataBuilder {

  private static int instanceNumber = 0;

  private UUID id;
  private String name;
  private String code;

  /**
   * Builds instance of {@link RejectionReasonCategoryDataBuilder} with sample data.
   */
  public RejectionReasonCategoryDataBuilder() {
    instanceNumber++;

    id = UUID.randomUUID();
    name = "RejectionReasonCategory" + instanceNumber;

  }

  /**
   * Builds instance of {@link org.openlmis.requisition.domain.RejectionReasonCategory}.
   */
  public RejectionReasonCategory build() {
    RejectionReasonCategory rejectionReasonCategory =
            RejectionReasonCategory.newRejectionReasonCategory(name, code);
    rejectionReasonCategory.setId(id);
    return rejectionReasonCategory;
  }

  /**
   * Builds instance of {@link RejectionReasonCategory} without id.
   */
  public RejectionReasonCategory buildAsNew() {
    return this.withoutId().build();
  }

  public RejectionReasonCategoryDataBuilder withoutId() {
    this.id = null;
    return this;
  }

  public RejectionReasonCategoryDataBuilder withName(String name) {
    this.name = name;
    return this;
  }

  public RejectionReasonCategoryDataBuilder withCode(String code) {
    this.code = code;
    return this;
  }
}
