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

import java.util.UUID;

import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;

public interface QuantificationExtractRepository extends
    PagingAndSortingRepository<RequisitionLineItem, UUID>,
    BaseAuditableRepository<RequisitionLineItem, UUID> {

  @Query(value = "SELECT\n"
      + "    r.*\n"
      + "FROM\n"
      + "    requisition.requisition_line_items r\n"
      + " ",
      nativeQuery = true)
  Page<RequisitionLineItem> findAllWithoutSnapshots(Pageable pageable);
}
