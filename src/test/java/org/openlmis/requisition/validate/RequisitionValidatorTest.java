package org.openlmis.requisition.validate;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.validation.Errors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionValidatorTest {

  private static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";
  private static final String STOCK_ON_HAND = "stockOnHand";
  private static final String REQUESTED_QUANTITY = "requestedQuantity";
  private static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

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
    columnsMap = initiateColumns();
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setColumnsMap(columnsMap);
    mockRepositoriesAndObjects();
  }

  @Test
  public void shouldRejectIfRequisitionItemsEmpty() {
    requisitionLineItems = new ArrayList<>();

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(REQUISITION_LINE_ITEMS),
        eq("A requisitionLineItems" + RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION));
  }

  @Test
  public void shouldRejectIfValueIsLessThanZero() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(-1);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(REQUISITION_LINE_ITEMS),
        eq("Quantity" + RequisitionValidator.VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION));
  }

  @Test
  public void shouldRejectIfValueIsNull() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(REQUISITION_LINE_ITEMS),
        eq("Quantity" + RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION));
  }

  @Test
  public void shouldRejectIfColumnDoesNotExist() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(1);
    requisitionLineItems.add(lineItem);

    columnsMap.remove(REQUESTED_QUANTITY);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.TEMPLATE_DOESNT_CONTAIN_FIELD));
  }

  @Test
  public void shouldRejectIfColumnIsCalculatedAndValueNotEmpty() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(1);
    requisitionLineItems.add(lineItem);

    columnsMap.get(STOCK_ON_HAND).setSource(SourceType.CALCULATED);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.TEMPLATE_COLUMN_IS_CALCULATED));
  }

  @Test
  public void shouldRejectIfColumnIsHiddenAndValueNotEmpty() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(1);
    requisitionLineItems.add(lineItem);

    columnsMap.get(STOCK_ON_HAND).setIsDisplayed(false);

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.TEMPLATE_COLUMN_IS_HIDDEN));
  }
  
  private Map<String, RequisitionTemplateColumn> initiateColumns() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();
    columns.put(REQUESTED_QUANTITY,
        generateTemplateColumn(REQUESTED_QUANTITY, SourceType.USER_INPUT, "J"));
    columns.put(TOTAL_RECEIVED_QUANTITY,
        generateTemplateColumn(TOTAL_RECEIVED_QUANTITY, SourceType.USER_INPUT, "B"));
    columns.put(STOCK_ON_HAND,
        generateTemplateColumn(STOCK_ON_HAND, SourceType.USER_INPUT, "E"));
    return columns;
  }

  private RequisitionLineItem generateLineItem() {
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setRequestedQuantity(1);
    lineItem.setBeginningBalance(1);
    lineItem.setTotalReceivedQuantity(1);
    lineItem.setStockOnHand(0);
    lineItem.setTotalConsumedQuantity(0);
    lineItem.setTotalLossesAndAdjustments(0);
    return lineItem;
  }

  private RequisitionTemplateColumn generateTemplateColumn(String name, SourceType sourceType,
                                                           String indicator) {
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setName(name);
    columnDefinition.setIndicator(indicator);

    RequisitionTemplateColumn requisitionTemplateColumn = new RequisitionTemplateColumn();
    requisitionTemplateColumn.setColumnDefinition(columnDefinition);
    requisitionTemplateColumn.setSource(sourceType);
    requisitionTemplateColumn.setName(name);
    requisitionTemplateColumn.setIsDisplayed(true);
    return requisitionTemplateColumn;
  }

  private void mockRepositoriesAndObjects() {
    UUID programId = UUID.randomUUID();

    when(requisition.getProgram()).thenReturn(programId);
    when(requisition.getRequisitionLineItems()).thenReturn(requisitionLineItems);

    when(requisitionTemplateRepository
        .getTemplateForProgram(programId)).thenReturn(requisitionTemplate);
  }
}
