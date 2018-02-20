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

package org.openlmis.requisition.domain.requisition;

import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ORDERABLE_NOT_IN_AVAILABLE_LIST;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.RequisitionDataBuilder;
import org.openlmis.requisition.testutils.RequisitionLineItemDataBuilder;
import org.openlmis.requisition.utils.Message;

import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@RunWith(MockitoJUnitRunner.class)
public class AvailableOrderableValidatorTest {

  @Mock
  private OrderableReferenceDataService orderableReferenceDataService;

  private Requisition requisitionUpdater = new RequisitionDataBuilder()
      .addLineItem(new RequisitionLineItemDataBuilder().build())
      .addLineItem(new RequisitionLineItemDataBuilder().build())
      .addLineItem(new RequisitionLineItemDataBuilder().build())
      .build();

  private Requisition requisitionToUpdate = new RequisitionDataBuilder()
      .addLineItem(new RequisitionLineItemDataBuilder().build())
      .build();

  private AvailableOrderableValidator validator;

  private Map<String, Message> errors = Maps.newHashMap();

  @Before
  public void setUp() throws Exception {
    validator = new AvailableOrderableValidator(
        orderableReferenceDataService, requisitionUpdater, requisitionToUpdate
    );
  }

  @Test
  public void shouldRejectIfOrderableIsNotPresent() {
    when(orderableReferenceDataService.findByIds(anySetOf(UUID.class)))
        .thenReturn(Lists.newArrayList(new OrderableDtoDataBuilder().build()));

    validator.validate(errors);

    verify(orderableReferenceDataService).findByIds(anySetOf(UUID.class));
    assertThat(
        errors,
        hasEntry(
            is(REQUISITION_LINE_ITEMS),
            hasProperty("key", is(ERROR_ORDERABLE_NOT_IN_AVAILABLE_LIST))
        )
    );
  }

  @Test
  public void shouldValidate() {
    requisitionToUpdate.setAvailableProducts(requisitionUpdater
        .getRequisitionLineItems()
        .stream()
        .map(RequisitionLineItem::getOrderableId)
        .collect(Collectors.toSet()));

    validator.validate(errors);

    verifyZeroInteractions(orderableReferenceDataService);
    assertThat(errors.entrySet(), hasSize(0));
  }
}
