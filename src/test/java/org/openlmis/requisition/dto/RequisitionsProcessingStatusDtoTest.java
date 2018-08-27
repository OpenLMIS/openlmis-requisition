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

package org.openlmis.requisition.dto;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionsProcessingStatusDtoTest {
  private OrderableDto orderable1;
  private OrderableDto orderable2;
  private OrderableDto orderable3;

  private ApproveRequisitionDto requisition1;
  private ApproveRequisitionDto requisition2;
  private ApproveRequisitionDto requisition3;

  RequisitionsProcessingStatusDto processingStatus;

  @Before
  public void setUp() throws Exception {
    orderable1 = new OrderableDto();
    orderable1.setId(UUID.randomUUID());

    orderable2 = new OrderableDto();
    orderable2.setId(UUID.randomUUID());

    orderable3 = new OrderableDto();
    orderable3.setId(UUID.randomUUID());

    requisition1 = create(UUID.randomUUID(), orderable1, false, orderable2, true, orderable3);
    requisition2 = create(UUID.randomUUID(), orderable1, true, orderable2, false, orderable3);
    requisition3 = create(UUID.randomUUID(), orderable1, false, orderable2, false, orderable3);

    processingStatus = new RequisitionsProcessingStatusDto();
    processingStatus.addProcessedRequisition(requisition1);
    processingStatus.addProcessedRequisition(requisition2);
    processingStatus.addProcessedRequisition(requisition3);
  }

  @Test
  public void shouldRemoveLineItemsIfProductIsSkippedInAllRequisitions() {
    processingStatus.removeSkippedProducts();

    Set<ApproveRequisitionDto> requisitions = processingStatus.getRequisitionDtos();

    assertFalse(requisitions.isEmpty());
    assertEquals(3, requisitions.size());

    for (ApproveRequisitionDto requisition : requisitions) {
      List<ApproveRequisitionLineItemDto> lineItems = requisition.getRequisitionLineItems();

      assertEquals(2, lineItems.size());
      assertEquals(lineItems.get(0).getOrderable().getId(), orderable1.getId());
      assertEquals(lineItems.get(1).getOrderable().getId(), orderable2.getId());
    }
  }

  private ApproveRequisitionDto create(UUID key, OrderableDto orderable1, boolean skipped1,
                                OrderableDto orderable2, boolean skipped2,
                                OrderableDto orderable3) {
    ApproveRequisitionDto dto = new ApproveRequisitionDto();
    dto.setId(key);

    ApproveRequisitionLineItemDto line1 = new ApproveRequisitionLineItemDto();
    line1.setOrderable(orderable1);
    line1.setSkipped(skipped1);

    ApproveRequisitionLineItemDto line2 = new ApproveRequisitionLineItemDto();
    line2.setOrderable(orderable2);
    line2.setSkipped(skipped2);

    ApproveRequisitionLineItemDto line3 = new ApproveRequisitionLineItemDto();
    line3.setOrderable(orderable3);
    line3.setSkipped(true);

    dto.setRequisitionLineItems(Lists.newArrayList(line1, line2, line3));

    return dto;
  }

}