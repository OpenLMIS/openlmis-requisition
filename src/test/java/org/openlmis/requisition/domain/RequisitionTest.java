package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.verifyStatic;

import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;

@PrepareForTest({LineItemFieldsCalculator.class})
@RunWith(PowerMockRunner.class)
public class RequisitionTest {

  private Requisition requisition;
  private RequisitionLineItem requisitionLineItem;

  private UUID productId = UUID.randomUUID();

  @Before
  public void setUp() {
    requisition = new Requisition();
    requisitionLineItem = new RequisitionLineItem();

    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setRequestedQuantity(10);
    requisitionLineItem.setStockOnHand(20);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(productId);

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));
  }

  @Test
  public void shouldAuthorizeRequisitionIfItStatusIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingRequisitionWithNotSubmittedStatus()
      throws RequisitionException {
    requisition.authorize();
  }

  @Test
  public void shouldCalculateStockOnHandForRequisitionLineItemsWhenAuthorizing()
      throws RequisitionException, RequisitionTemplateColumnException {
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    mockStatic(LineItemFieldsCalculator.class);
    RequisitionLineItem requisitionLineItem = mock(RequisitionLineItem.class);

    requisition.setRequisitionLineItems(new ArrayList<>(
        Collections.singletonList(requisitionLineItem)));

    when(requisitionTemplate.isColumnDisplayed("stockOnHand")).thenReturn(true);
    when(requisitionTemplate.isColumnCalculated("stockOnHand")).thenReturn(true);
    when(LineItemFieldsCalculator.calculateTotal(requisitionLineItem))
        .thenReturn(1);

    Requisition newRequisition = new Requisition();

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();
    requisition.updateFrom(newRequisition, requisitionTemplate, Lists.newArrayList());

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
    verifyStatic(times(1));
  }

  @Test
  public void shouldCalculateTotalValueWhenUpdatingRequisition()
      throws RequisitionException, RequisitionTemplateColumnException {
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    mockStatic(LineItemFieldsCalculator.class);
    RequisitionLineItem requisitionLineItem = mock(RequisitionLineItem.class);

    when(requisitionTemplate.isColumnDisplayed("total")).thenReturn(true);
    when(LineItemFieldsCalculator.calculateStockOnHand(requisitionLineItem))
        .thenReturn(1);

    requisition.setRequisitionLineItems(new ArrayList<>(
        Collections.singletonList(requisitionLineItem)));

    Requisition newRequisition = new Requisition();

    requisition.updateFrom(newRequisition, requisitionTemplate, Lists.newArrayList());
    verifyStatic(times(1));
  }

  @Test
  public void shouldAddOnlyAddNonFullSupplyLines() throws Exception {
    requisition.getRequisitionLineItems().clear();

    RequisitionTemplate template = mock(RequisitionTemplate.class);

    RequisitionLineItem fullSupply = new RequisitionLineItem();
    RequisitionLineItem nonFullSupply = new RequisitionLineItem();
    nonFullSupply.setNonFullSupply(true);

    Requisition newRequisition = new Requisition();
    newRequisition.setRequisitionLineItems(Lists.newArrayList(fullSupply, nonFullSupply));

    requisition.updateFrom(newRequisition, template, Lists.newArrayList());

    assertThat(requisition.getRequisitionLineItems(), hasSize(1));

    RequisitionLineItem item = requisition.getRequisitionLineItems().get(0);
    assertThat(item.isNonFullSupply(), is(true));
  }

  @Test
  public void shouldNotRemoveFullSupplyLines() throws Exception {
    RequisitionTemplate template = mock(RequisitionTemplate.class);

    RequisitionLineItem nonFullSupply = new RequisitionLineItem();
    nonFullSupply.setNonFullSupply(true);

    Requisition newRequisition = new Requisition();
    newRequisition.setRequisitionLineItems(Lists.newArrayList(nonFullSupply));

    int count = requisition.getRequisitionLineItems().size();
    requisition.updateFrom(newRequisition, template, Lists.newArrayList());

    assertThat(requisition.getRequisitionLineItems(), hasSize(count + 1));

    assertThat(
        requisition.getRequisitionLineItems().stream()
            .filter(RequisitionLineItem::isNonFullSupply).count(),
        is(1L)
    );

    assertThat(
        requisition.getRequisitionLineItems().stream()
            .filter(line -> !line.isNonFullSupply()).count(),
        is((long) count)
    );
  }

  @Test
  public void shouldFindRequisitionLineItemByProductId() {
    RequisitionLineItem found = requisition.findLineByProductId(productId);

    assertNotNull(found);
    assertEquals(requisitionLineItem, found);
  }

  @Test
  public void shouldSetRequisitionFieldForLineItemsAfterUpdate() throws Exception {
    // given
    Requisition newRequisition = new Requisition();

    // existing line
    RequisitionLineItem firstRequisitionLineItem = new RequisitionLineItem();
    firstRequisitionLineItem.setId(requisitionLineItem.getId());
    firstRequisitionLineItem.setRequestedQuantity(10);
    firstRequisitionLineItem.setStockOnHand(20);
    firstRequisitionLineItem.setRequisition(newRequisition);
    firstRequisitionLineItem.setOrderableProductId(productId);

    // new line
    RequisitionLineItem secondRequisitionLineItem = new RequisitionLineItem();
    secondRequisitionLineItem.setRequestedQuantity(10);
    secondRequisitionLineItem.setStockOnHand(20);
    secondRequisitionLineItem.setRequisition(newRequisition);
    secondRequisitionLineItem.setOrderableProductId(productId);

    newRequisition.setId(UUID.randomUUID());
    newRequisition.setStatus(RequisitionStatus.INITIATED);
    newRequisition.setRequisitionLineItems(
        Lists.newArrayList(firstRequisitionLineItem, secondRequisitionLineItem)
    );

    // when
    requisition.setId(UUID.randomUUID());
    requisition.updateFrom(newRequisition, mock(RequisitionTemplate.class), Lists.newArrayList());

    // then
    requisition
        .getRequisitionLineItems()
        .forEach(line -> assertThat(line.getRequisition().getId(), is(requisition.getId())));
  }

  @Test
  public void shouldSetNullForCalculatedValuesIfColumnIsHidden() throws Exception {
    requisitionLineItem.setStockOnHand(10);
    requisitionLineItem.setTotalConsumedQuantity(15);

    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);

    when(requisitionTemplate.isColumnDisplayed("stockOnHand")).thenReturn(false);
    when(requisitionTemplate.isColumnDisplayed("totalConsumedQuantity")).thenReturn(false);

    Requisition newRequisition = new Requisition();
    requisition.updateFrom(newRequisition, requisitionTemplate, Lists.newArrayList());

    assertThat(requisitionLineItem.getStockOnHand(), is(nullValue()));
    assertThat(requisitionLineItem.getTotalConsumedQuantity(), is(nullValue()));

  }
}
