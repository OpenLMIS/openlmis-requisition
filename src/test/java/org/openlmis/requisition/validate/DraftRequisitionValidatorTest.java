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

package org.openlmis.requisition.validate;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FIELD_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_INVARIANT;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ONLY_AVAILABLE_FOR_APPROVAL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.validation.Errors;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public class DraftRequisitionValidatorTest {

  @Mock
  private MessageService messageService;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @InjectMocks
  private DraftRequisitionValidator draftRequisitionValidator;

  private Errors errors = mock(Errors.class);

  private List<RequisitionLineItem> requisitionLineItems;
  private RequisitionTemplate requisitionTemplate;
  private Map<String, RequisitionTemplateColumn> columnsMap;
  private Requisition requisition;
  private UUID requisitionId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID processingPeriodId = UUID.randomUUID();
  private UUID supervisoryNodeId = UUID.randomUUID();

  @Before
  public void setUp() {
    requisitionLineItems = new ArrayList<>();
    columnsMap = RequisitionValidationTestUtils.initiateColumns();
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setId(UUID.randomUUID());
    requisitionTemplate.setColumnsMap(columnsMap);
    requisition = generateRequisition();
    mockRepositoriesAndObjects();
  }

  @Test
  public void shouldRejectIfInvariantValueChanged() {
    Requisition updatedRequisition = generateRequisition();

    updatedRequisition.setProgramId(UUID.randomUUID());
    Assert.assertNotEquals(requisition.getProgramId(), updatedRequisition.getProgramId());
    updatedRequisition.setFacilityId(UUID.randomUUID());
    Assert.assertNotEquals(requisition.getFacilityId(), updatedRequisition.getFacilityId());
    updatedRequisition.setProcessingPeriodId(UUID.randomUUID());
    Assert.assertNotEquals(requisition.getProcessingPeriodId(),
        updatedRequisition.getProcessingPeriodId());
    updatedRequisition.setSupervisoryNodeId(UUID.randomUUID());
    Assert.assertNotEquals(requisition.getSupervisoryNodeId(),
        updatedRequisition.getSupervisoryNodeId());
    updatedRequisition.setEmergency(false);

    Message message1 = new Message(ERROR_IS_INVARIANT, Requisition.PROGRAM_ID);
    Message message2 = new Message(ERROR_IS_INVARIANT, Requisition.FACILITY_ID);
    Message message3 = new Message(ERROR_IS_INVARIANT, Requisition.PROCESSING_PERIOD_ID);
    Message message4 = new Message(ERROR_IS_INVARIANT, Requisition.EMERGENCY);
    Message message6 = new Message(ERROR_IS_INVARIANT, Requisition.SUPERVISORY_NODE_ID);

    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        ERROR_IS_INVARIANT));
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        ERROR_IS_INVARIANT));
    when(messageService.localize(message3)).thenReturn(message3.new LocalizedMessage(
        ERROR_IS_INVARIANT));
    when(messageService.localize(message4)).thenReturn(message4.new LocalizedMessage(
        ERROR_IS_INVARIANT));
    when(messageService.localize(message6)).thenReturn(message6.new LocalizedMessage(
        ERROR_IS_INVARIANT));

    draftRequisitionValidator.validate(updatedRequisition, errors);

    verify(errors).rejectValue(eq(Requisition.PROGRAM_ID),
        contains(ERROR_IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.FACILITY_ID),
        contains(ERROR_IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.PROCESSING_PERIOD_ID),
        contains(ERROR_IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.EMERGENCY),
        contains(ERROR_IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.SUPERVISORY_NODE_ID),
        contains(ERROR_IS_INVARIANT));
  }

  @Test
  public void shouldRejectIfColumnIsCalculatedAndValueNotEmpty() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(1);
    requisitionLineItems.add(lineItem);

    requisitionTemplate.changeColumnSource(RequisitionLineItem.STOCK_ON_HAND,
        SourceType.CALCULATED);

    Message message = new Message(ERROR_FIELD_IS_CALCULATED, RequisitionLineItem.STOCK_ON_HAND);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionLineItem.STOCK_ON_HAND + " is calculated and should not contain a value."));
    Message message1 = new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
        RequisitionLineItem.APPROVED_QUANTITY);
    Message message2 = new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
        RequisitionLineItem.REMARKS_COLUMN);

    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionLineItem.APPROVED_QUANTITY + "is only available during the approval step of "
            + "the requisition process."));
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        RequisitionLineItem.REMARKS_COLUMN + "is only available during the approval step of the "
            + "requisition process."));

    draftRequisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(ERROR_FIELD_IS_CALCULATED));
  }


  @Test
  public void shouldRejectIfNumberOfTotalStockoutDaysIsGreaterThanLengthOfPeriod() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setTotalStockoutDays(111111);
    requisitionLineItems.add(lineItem);

    Message message =  new Message(ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD));

    draftRequisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD));
  }

  @Test
  public void shouldRejectIfValueIsPresentWithInvalidRequisitionStatus() {
    requisition.setStatus(RequisitionStatus.INITIATED);
    RequisitionLineItem lineItem = getInvalidRequisitionLineItemForInitializedStatus();

    requisitionLineItems.add(lineItem);

    Message message1 = new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
        RequisitionLineItem.APPROVED_QUANTITY);
    Message message2 = new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
        RequisitionLineItem.REMARKS_COLUMN);

    when(messageService.localize(message1))
        .thenReturn(message1.new LocalizedMessage(
            RequisitionLineItem.APPROVED_QUANTITY + " is only available during the approval step of"
                + " the requisition process."))
        .thenReturn(message2.new LocalizedMessage(
        RequisitionLineItem.REMARKS_COLUMN + " is only available during the approval step of the "
            + "requisition process."));

    draftRequisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionLineItem.APPROVED_QUANTITY
            + " is only available during the approval step of the requisition process."));

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionLineItem.REMARKS_COLUMN
            + " is only available during the approval step of the requisition process."));
  }

  @Test
  public void shouldNotValidateSkippedLineItems() {
    requisition.setStatus(RequisitionStatus.INITIATED);
    RequisitionLineItem lineItem = getInvalidRequisitionLineItemForInitializedStatus();
    lineItem.setSkipped(true);

    RequisitionLineItem lineItem2 = generateLineItem();

    requisitionLineItems.add(lineItem);
    requisitionLineItems.add(lineItem2);

    Message message1 = new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
        RequisitionLineItem.APPROVED_QUANTITY);
    Message message2 = new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
        RequisitionLineItem.REMARKS_COLUMN);

    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionLineItem.APPROVED_QUANTITY + "is only available during the approval step of "
            + "the requisition process."));
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        RequisitionLineItem.REMARKS_COLUMN + "is only available during the approval step of the "
            + "requisition process."));

    draftRequisitionValidator.validate(requisition, errors);

    verify(errors, times(0)).rejectValue(any(), any());
  }

  private RequisitionLineItem generateLineItem() {
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setRequestedQuantity(1);
    lineItem.setBeginningBalance(1);
    lineItem.setTotalReceivedQuantity(1);
    lineItem.setStockOnHand(0);
    lineItem.setTotalConsumedQuantity(0);
    lineItem.setTotalLossesAndAdjustments(0);
    lineItem.setRequisition(requisition);
    return lineItem;
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition(facilityId, programId, processingPeriodId,
        RequisitionStatus.AUTHORIZED, true);
    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition.setId(requisitionId);
    requisition.setTemplate(requisitionTemplate);
    requisition.setSupervisoryNodeId(supervisoryNodeId);
    requisition.setNumberOfMonthsInPeriod(1);
    return requisition;
  }

  private void mockRepositoriesAndObjects() {
    when(configurationSettingService.getBoolValue("skipAuthorization")).thenReturn(false);

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);
  }

  private RequisitionLineItem getInvalidRequisitionLineItemForInitializedStatus() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setApprovedQuantity(1);
    lineItem.setRemarks("Remarks");
    return lineItem;
  }
}
