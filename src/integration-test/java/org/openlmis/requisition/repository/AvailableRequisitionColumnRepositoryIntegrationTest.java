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
import static org.junit.Assert.assertTrue;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_LOSSES_AND_ADJUSTMENTS;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_RECEIVED_QUANTITY;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.repository.CrudRepository;

public class AvailableRequisitionColumnRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<AvailableRequisitionColumn> {

  @Autowired
  private AvailableRequisitionColumnRepository repository;

  @Override
  CrudRepository<AvailableRequisitionColumn, UUID> getRepository() {
    return repository;
  }

  @Override
  AvailableRequisitionColumn generateInstance() {
    return new AvailableRequisitionColumnDataBuilder().withoutId().withoutOptions().build();
  }

  @Test
  public void shouldFindColumnsWithTagRequired() {
    List<AvailableRequisitionColumn> columns = repository.findBySupportsTag(true);

    assertEquals(3, columns.size());
    List<String> columnNames = columns.stream()
        .map(AvailableRequisitionColumn::getName)
        .collect(Collectors.toList());

    assertTrue(columnNames.contains(TOTAL_CONSUMED_QUANTITY));
    assertTrue(columnNames.contains(TOTAL_LOSSES_AND_ADJUSTMENTS));
    assertTrue(columnNames.contains(TOTAL_RECEIVED_QUANTITY));
  }
}
