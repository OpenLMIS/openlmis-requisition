package org.openlmis.requisition.validate;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.validation.Errors;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class RequisitionValidatorTest {

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @InjectMocks
  private RequisitionValidator requisitionValidator;

  private Requisition requisition = mock(Requisition.class);
  private Errors errors = mock(Errors.class);

  private List<RequisitionLineItem> requisitionLineItems;
  private RequisitionTemplate requisitionTemplate;
  private Map<String, RequisitionTemplateColumn> columnsMap;

  @Before
  public void setUp() {
    requisitionLineItems = new ArrayList<>();
    columnsMap = RequisitionValidationTestUtils.initiateColumns();
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setColumnsMap(columnsMap);
    mockRepositoriesAndObjects();
  }

  @Test
  public void shouldRejectIfRequisitionItemsEmpty() {
    requisitionLineItems = new ArrayList<>();

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        eq("A requisitionLineItems" + RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION));
  }

  @Test
  public void shouldRejectIfValueIsLessThanZero() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(-1);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        eq(RequisitionLineItem.REQUESTED_QUANTITY
            + RequisitionValidator.VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION));
  }

  @Test
  public void shouldRejectIfValueIsNull() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        eq(RequisitionLineItem.REQUESTED_QUANTITY
            + RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION));
  }

  @Test
  public void shouldRejectIfColumnDoesNotExist() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(1);
    requisitionLineItems.add(lineItem);

    columnsMap.remove(RequisitionLineItem.STOCK_ON_HAND);

    requisitionValidator.validate(requisition, errors);

    // 1. when we check if value is not null
    // 2. when we check if values is greater or equal to zero
    verify(errors, times(2)).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains("is not present in template"));
  }

  @Test
  public void shouldRejectIfColumnIsHiddenAndValueNotEmpty() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(1);
    requisitionLineItems.add(lineItem);

    columnsMap.get(RequisitionLineItem.STOCK_ON_HAND).setIsDisplayed(false);

    requisitionValidator.validate(requisition, errors);

    // 1. when we check if value is not null
    // 2. when we check if values is greater or equal to zero
    verify(errors, times(2)).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.TEMPLATE_COLUMN_IS_HIDDEN));
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

  private void mockRepositoriesAndObjects() {
    UUID programId = UUID.randomUUID();

    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getRequisitionLineItems()).thenReturn(requisitionLineItems);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);

    when(requisitionTemplateRepository
        .getTemplateForProgram(programId)).thenReturn(requisitionTemplate);
    when(requisitionRepository.findOne(any())).thenReturn(requisition);
    when(configurationSettingService.getBoolValue("skipAuthorization")).thenReturn(false);
  }
}
