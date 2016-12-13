package org.openlmis.requisition.validate;

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
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.validation.Errors;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class DraftRequisitionValidatorTest {

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

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

  @Before
  public void setUp() {
    requisitionLineItems = new ArrayList<>();
    columnsMap = RequisitionValidationTestUtils.initiateColumns();
    requisitionTemplate = new RequisitionTemplate();
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
    updatedRequisition.setEmergency(false);

    draftRequisitionValidator.validate(updatedRequisition, errors);

    verify(errors).rejectValue(eq(Requisition.PROGRAM_ID),
        contains(RequisitionValidator.IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.FACILITY_ID),
        contains(RequisitionValidator.IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.PROCESSING_PERIOD_ID),
        contains(RequisitionValidator.IS_INVARIANT));
    verify(errors).rejectValue(eq(Requisition.EMERGENCY),
        contains(RequisitionValidator.IS_INVARIANT));
  }

  @Test
  public void shouldRejectIfColumnIsCalculatedAndValueNotEmpty() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(1);
    requisitionLineItems.add(lineItem);

    columnsMap.get(RequisitionLineItem.STOCK_ON_HAND).setSource(SourceType.CALCULATED);

    draftRequisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.TEMPLATE_COLUMN_IS_CALCULATED));
  }

  @Test
  public void shouldRejectIfValueIsPresentWithInvalidRequisitionStatus() {
    requisition.setStatus(RequisitionStatus.INITIATED);

    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setApprovedQuantity(1);
    lineItem.setRemarks("Remarks");
    requisitionLineItems.add(lineItem);

    draftRequisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        eq(RequisitionLineItem.APPROVED_QUANTITY
            + RequisitionValidator.IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP));

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        eq(RequisitionLineItem.REMARKS
            + RequisitionValidator.IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP));
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
    return requisition;
  }

  private void mockRepositoriesAndObjects() {
    when(requisitionTemplateRepository
        .getTemplateForProgram(programId)).thenReturn(requisitionTemplate);
    when(configurationSettingService.getBoolValue("skipAuthorization")).thenReturn(false);

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);
  }
}
