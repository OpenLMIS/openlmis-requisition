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
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;
import static org.openlmis.requisition.domain.requisition.Requisition.EMERGENCY_FIELD;
import static org.openlmis.requisition.domain.requisition.Requisition.FACILITY_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.PROCESSING_PERIOD_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.PROGRAM_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.domain.requisition.Requisition.SUPERVISORY_NODE_ID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_INVARIANT;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_LINE_ITEM_ADDED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_LINE_ITEM_REMOVED;

import com.google.common.collect.Lists;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.utils.Message;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class RequisitionInvariantsValidatorTest {
  private Requisition requisitionToUpdate;
  private Requisition requisitionUpdater;
  private RequisitionInvariantsValidator validator;
  private Map<String, Message> errors;

  @Before
  public void setUp() {
    requisitionToUpdate = new RequisitionDataBuilder()
        .addLineItem(new RequisitionLineItemDataBuilder().build())
        .withSupervisoryNodeId(UUID.randomUUID())
        .build();

    requisitionUpdater = new RequisitionDataBuilder()
        .withFacilityId(requisitionToUpdate.getFacilityId())
        .withProgramId(requisitionToUpdate.getProgramId())
        .withProcessingPeriodId(requisitionToUpdate.getProcessingPeriodId())
        .withSupervisoryNodeId(requisitionToUpdate.getSupervisoryNodeId())
        .withLineItems(requisitionToUpdate.getRequisitionLineItems())
        .build();

    validator = new RequisitionInvariantsValidator(
        requisitionUpdater, requisitionToUpdate
    );

    errors = new HashMap<>();
  }

  @Test
  public void shouldValidate() {
    validator.validateCanUpdate(errors);
    assertThat(errors.entrySet(), hasSize(0));
  }

  @Test
  public void shouldRejectIfFacilityIdWasChanged() {
    requisitionUpdater.setFacilityId(UUID.randomUUID());

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(FACILITY_ID, new Message(ERROR_IS_INVARIANT)));
  }

  @Test
  public void shouldRejectIfProgramIdWasChanged() {
    requisitionUpdater.setProgramId(UUID.randomUUID());

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(PROGRAM_ID, new Message(ERROR_IS_INVARIANT)));
  }

  @Test
  public void shouldRejectIfProcessingPeriodIdWasChanged() {
    requisitionUpdater.setProcessingPeriodId(UUID.randomUUID());

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(PROCESSING_PERIOD_ID, new Message(ERROR_IS_INVARIANT)));
  }

  @Test
  public void shouldRejectIfEmergencyFacilityIdWasChanged() {
    requisitionUpdater.setEmergency(true);

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(EMERGENCY_FIELD, new Message(ERROR_IS_INVARIANT)));
  }

  @Test
  public void shouldRejectIfSupervisoryNodeIdWasChanged() {
    requisitionUpdater.setSupervisoryNodeId(UUID.randomUUID());

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(SUPERVISORY_NODE_ID, new Message(ERROR_IS_INVARIANT)));
  }

  @Test
  public void shouldRejectIfOrderableIdHasBeenChanged() {
    requisitionUpdater.setRequisitionLineItems(Lists.newArrayList(
        new RequisitionLineItemDataBuilder()
            .withId(requisitionToUpdate.getRequisitionLineItems().get(0).getId())
            .build()
    ));

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(REQUISITION_LINE_ITEMS, new Message(ERROR_IS_INVARIANT)));
  }

  @Test
  public void shouldRejectIfFullSupplyLineWasRemovedForRegularRequisition() {
    requisitionUpdater.setRequisitionLineItems(Lists.newArrayList());

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(REQUISITION_LINE_ITEMS, new Message(ERROR_LINE_ITEM_REMOVED)));
  }

  @Test
  public void shouldRejectIfNewFullSupplyLineWasAddedForRegularRequisition() {
    requisitionUpdater
        .getRequisitionLineItems()
        .add(new RequisitionLineItemDataBuilder().build());

    validator.validateCanUpdate(errors);

    assertThat(errors, hasEntry(REQUISITION_LINE_ITEMS, new Message(ERROR_LINE_ITEM_ADDED)));
  }

}
