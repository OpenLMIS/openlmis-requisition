package org.openlmis.requisition.domain;

import static java.util.Arrays.asList;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.verifyStatic;

import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@PrepareForTest({LineItemFieldsCalculator.class})
@RunWith(PowerMockRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTest {
  private static final UUID ORDERABLE_PRODUCT_ID = UUID.randomUUID();

  private static final Money PRICE_PER_PACK = new Money("9");
  private static final int ADJUSTED_CONSUMPTION = 1;
  private static final int AVERAGE_CONSUMPTION = 1;
  private static final Money TOTAL_COST = new Money("5");
  private static final int MONTHS_IN_PERIOD = 1;

  private Requisition requisition;
  private RequisitionLineItem requisitionLineItem;

  private UUID productId = UUID.randomUUID();

  @Mock
  private RequisitionTemplate template;

  @Before
  public void setUp() {
    requisition = new Requisition();
    requisitionLineItem = new RequisitionLineItem();

    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setRequestedQuantity(10);
    requisitionLineItem.setStockOnHand(20);
    requisitionLineItem.setPricePerPack(PRICE_PER_PACK);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(productId);

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));
    requisition.setNumberOfMonthsInPeriod(MONTHS_IN_PERIOD);
  }

  @Test
  public void shouldAuthorizeRequisitionIfItStatusIsSubmitted() {
    requisition.setTemplate(mock(RequisitionTemplate.class));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize();

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenAuthorizingRequisitionWithNotSubmittedStatus() {
    requisition.setTemplate(mock(RequisitionTemplate.class));
    requisition.authorize();
  }

  @Test
  public void shouldCalculateStockOnHandForRequisitionLineItemsWhenAuthorizing() {
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    PowerMockito.spy(LineItemFieldsCalculator.class);
    RequisitionLineItem requisitionLineItem = mock(RequisitionLineItem.class);

    requisition.setRequisitionLineItems(new ArrayList<>(
        Collections.singletonList(requisitionLineItem)));

    when(requisitionTemplate.isColumnDisplayed("stockOnHand")).thenReturn(true);
    when(requisitionTemplate.isColumnCalculated("stockOnHand")).thenReturn(true);
    when(LineItemFieldsCalculator.calculateTotal(requisitionLineItem))
        .thenReturn(1);

    requisition.setTemplate(requisitionTemplate);
    requisition.setStatus(RequisitionStatus.SUBMITTED);

    requisition.authorize();
    requisition.updateFrom(new Requisition(), Lists.newArrayList());

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
    verifyStatic(times(1));
  }

  @Test
  public void shouldCalculateTotalValueWhenUpdatingRequisition() {
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    PowerMockito.spy(LineItemFieldsCalculator.class);
    RequisitionLineItem requisitionLineItem = mock(RequisitionLineItem.class);

    when(requisitionTemplate.isColumnDisplayed("total")).thenReturn(true);
    when(LineItemFieldsCalculator.calculateStockOnHand(requisitionLineItem))
        .thenReturn(1);

    requisition.setRequisitionLineItems(new ArrayList<>(
        Collections.singletonList(requisitionLineItem)));

    requisition.setTemplate(requisitionTemplate);
    requisition.updateFrom(new Requisition(), Lists.newArrayList());
    verifyStatic(times(1));
  }

  @Test
  public void shouldAddOnlyAddNonFullSupplyLines() throws Exception {
    requisition.getRequisitionLineItems().clear();

    RequisitionLineItem fullSupply = new RequisitionLineItem();
    RequisitionLineItem nonFullSupply = new RequisitionLineItem();
    nonFullSupply.setNonFullSupply(true);

    Requisition newRequisition = new Requisition();
    newRequisition.setRequisitionLineItems(Lists.newArrayList(fullSupply, nonFullSupply));

    requisition.setTemplate(mock(RequisitionTemplate.class));
    requisition.updateFrom(newRequisition, Lists.newArrayList());

    assertThat(requisition.getRequisitionLineItems(), hasSize(1));

    RequisitionLineItem item = requisition.getRequisitionLineItems().get(0);
    assertThat(item.isNonFullSupply(), is(true));
  }

  @Test
  public void shouldNotRemoveFullSupplyLines() throws Exception {
    RequisitionLineItem nonFullSupply = new RequisitionLineItem();
    nonFullSupply.setNonFullSupply(true);

    Requisition newRequisition = new Requisition();
    newRequisition.setRequisitionLineItems(Lists.newArrayList(nonFullSupply));

    int count = requisition.getRequisitionLineItems().size();
    requisition.setTemplate(mock(RequisitionTemplate.class));
    requisition.updateFrom(newRequisition, Lists.newArrayList());

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
    requisition.setTemplate(template);
    requisition.setId(UUID.randomUUID());
    requisition.updateFrom(newRequisition, Lists.newArrayList());

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

    requisition.setTemplate(requisitionTemplate);
    requisition.updateFrom(new Requisition(), Lists.newArrayList());

    assertThat(requisitionLineItem.getStockOnHand(), is(nullValue()));
    assertThat(requisitionLineItem.getTotalConsumedQuantity(), is(nullValue()));

  }

  @Test
  public void shouldInitiateRequisitionLineItemFieldsIfPreviousRequisitionProvided() {
    // given
    final UUID productId1 = UUID.randomUUID();
    final UUID productId2 = UUID.randomUUID();

    Requisition previousRequisition = mock(Requisition.class);
    mockReqLine(previousRequisition, productId1, 10, 20); // 10 + 20 = 30 beginning balance
    mockReqLine(previousRequisition, productId2, 11, 22); // 11 + 22 = 33 beginning balance

    when(template.isColumnDisplayed(RequisitionLineItem.BEGINNING_BALANCE)).thenReturn(true);

    ApprovedProductDto product1 = mockApprovedProduct(productId1);
    ApprovedProductDto product2 = mockApprovedProduct(productId2);

    // when
    Requisition req = new Requisition();
    req.initiate(template, asList(product1, product2),
        Collections.singletonList(previousRequisition), 0);

    // then
    List<RequisitionLineItem> lineItems = req.getRequisitionLineItems();

    assertEquals(2, lineItems.size());
    assertThat(req.findLineByProductId(productId1).getBeginningBalance(), is(30));
    assertThat(req.findLineByProductId(productId2).getBeginningBalance(), is(33));
  }

  @Test
  public void shouldInitiateBeginningBalanceToZeroIfNoPreviousRequisition() {
    // given
    final UUID productId1 = UUID.randomUUID();
    final UUID productId2 = UUID.randomUUID();

    ApprovedProductDto product1 = mockApprovedProduct(productId1);
    ApprovedProductDto product2 = mockApprovedProduct(productId2);

    when(template.isColumnDisplayed(RequisitionLineItem.BEGINNING_BALANCE)).thenReturn(true);

    // when
    Requisition req = new Requisition();
    req.initiate(template, asList(product1, product2), Collections.emptyList(), 0);

    // then
    List<RequisitionLineItem> lineItems = req.getRequisitionLineItems();

    assertEquals(2, lineItems.size());
    assertThat(req.findLineByProductId(productId1).getBeginningBalance(), is(0));
    assertThat(req.findLineByProductId(productId2).getBeginningBalance(), is(0));
  }

  @Test
  public void shouldInitiateBeginningBalanceToZeroIfNotVisible() {
    // given
    final UUID productId1 = UUID.randomUUID();
    final UUID productId2 = UUID.randomUUID();

    Requisition previousRequisition = mock(Requisition.class);
    mockReqLine(previousRequisition, productId1, 10, 20); // 10 + 20 = 30 beginning balance
    mockReqLine(previousRequisition, productId2, 11, 22); // 11 + 22 = 33 beginning balance

    // should not initiate beginning balance because of this
    when(template.isColumnDisplayed(RequisitionLineItem.BEGINNING_BALANCE)).thenReturn(false);

    ApprovedProductDto product1 = mockApprovedProduct(productId1);
    ApprovedProductDto product2 = mockApprovedProduct(productId2);

    // when
    Requisition req = new Requisition();
    req.initiate(template, asList(product1, product2),
        Collections.singletonList(previousRequisition), 0);

    // then
    List<RequisitionLineItem> lineItems = req.getRequisitionLineItems();

    assertEquals(2, lineItems.size());
    assertThat(req.findLineByProductId(productId1).getBeginningBalance(), is(0));
    assertThat(req.findLineByProductId(productId2).getBeginningBalance(), is(0));
  }

  @Test
  public void shouldReturnNonSkippedRequisitionLineItems() {
    RequisitionLineItem notSkipped = getRequisitionLineItem(false);
    RequisitionLineItem skipped = getRequisitionLineItem(true);

    Requisition requisition = getRequisition(notSkipped, skipped);
    List<RequisitionLineItem> nonSkippedRequisitionLineItems =
        requisition.getNonSkippedRequisitionLineItems();
    RequisitionLineItem requisitionLineItem =
        nonSkippedRequisitionLineItems.get(0);

    assertEquals(1, nonSkippedRequisitionLineItems.size());
    assertEquals(notSkipped.getId(), requisitionLineItem.getId());
  }

  @Test
  public void shouldReturnSkippedRequisitionLineItems() {
    RequisitionLineItem notSkipped = getRequisitionLineItem(false);
    RequisitionLineItem skipped = getRequisitionLineItem(true);

    Requisition requisition = getRequisition(notSkipped, skipped);
    List<RequisitionLineItem> skippedRequisitionLineItems =
        requisition.getSkippedRequisitionLineItems();
    RequisitionLineItem requisitionLineItem =
        skippedRequisitionLineItems.get(0);

    assertEquals(1, skippedRequisitionLineItems.size());
    assertEquals(skipped.getId(), requisitionLineItem.getId());
  }

  @Test
  public void shouldCalculateAdjustedConsumptionTotalCostAndAverageConsumptionWhenSubmit() {
    // given
    prepareForTestAdjustedConcumptionTotalCostAndAverageConsumption();

    //when
    requisition.submit();

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
    assertEquals(AVERAGE_CONSUMPTION, requisitionLineItem.getAverageConsumption().longValue());
    assertEquals(TOTAL_COST, requisitionLineItem.getTotalCost());
  }

  @Test
  public void shouldCalculateAverageConsumptionWhenSubmitWithOnePreviousRequisition() {
    // given

    List<Integer> adjustedConsumptions = new ArrayList<>();
    adjustedConsumptions.add(5);
    prepareForTestAverageConsumption(adjustedConsumptions);
    when(LineItemFieldsCalculator
        .calculateAverageConsumption(Arrays.asList(5, ADJUSTED_CONSUMPTION)))
        .thenReturn(AVERAGE_CONSUMPTION);

    //when
    requisition.submit();

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
    assertEquals(AVERAGE_CONSUMPTION, requisitionLineItem.getAverageConsumption().longValue());
  }

  @Test
  public void shouldCalculateAverageConsumptionWhenSubmitWithManyPreviousRequisitions() {
    // given
    List<Integer> adjustedConsumptions = new ArrayList<>();
    adjustedConsumptions.add(5);
    adjustedConsumptions.add(5);
    adjustedConsumptions.add(5);
    prepareForTestAverageConsumption(adjustedConsumptions);
    when(LineItemFieldsCalculator
        .calculateAverageConsumption(Arrays.asList(5, 5, 5, ADJUSTED_CONSUMPTION)))
        .thenReturn(AVERAGE_CONSUMPTION);

    //when
    requisition.submit();

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
    assertEquals(AVERAGE_CONSUMPTION, requisitionLineItem.getAverageConsumption().longValue());
  }

  @Test
  public void shouldCalculateAdjustedConsumptionTotalCostAndAverageConsumptionWhenAuthorize() {
    // given
    prepareForTestAdjustedConcumptionTotalCostAndAverageConsumption();
    requisition.setStatus(RequisitionStatus.SUBMITTED);

    //when
    requisition.authorize();

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
    assertEquals(AVERAGE_CONSUMPTION, requisitionLineItem.getAverageConsumption().longValue());
    assertEquals(TOTAL_COST, requisitionLineItem.getTotalCost());
  }

  @Test
  public void shouldCalculateAdjustedConsumptionTotalCostAndAverageConsumptionWhenApprove() {
    // given
    prepareForTestAdjustedConcumptionTotalCostAndAverageConsumption();
    requisition.setStatus(RequisitionStatus.APPROVED);

    //when
    requisition.approve();

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
    assertEquals(AVERAGE_CONSUMPTION, requisitionLineItem.getAverageConsumption().longValue());
    assertEquals(TOTAL_COST, requisitionLineItem.getTotalCost());
  }

  @Test
  public void shouldSetPreviousAdjustedConsumptionsWhenOnePreviousRequisition() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);

    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(5);
    previousRequisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition.setRequisitionLineItems(
        Collections.singletonList(previousRequisitionLineItem));

    Requisition requisition = new Requisition();
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));

    requisition.setPreviousAdjustedConsumptions(
        Collections.singletonList(previousRequisition)
    );

    assertEquals(Collections.singletonList(5),
        requisitionLineItem.getPreviousAdjustedConsumptions());
  }

  @Test
  public void shouldSetPreviousAdjustedConsumptionsFromManyPreviousRequisitions() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);

    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(5);
    previousRequisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition
        .setRequisitionLineItems(Arrays.asList(previousRequisitionLineItem,
            previousRequisitionLineItem, previousRequisitionLineItem));

    Requisition requisition = new Requisition();
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));

    requisition.setPreviousAdjustedConsumptions(
        Collections.singletonList(previousRequisition)
    );

    assertEquals(Arrays.asList(5, 5, 5), requisitionLineItem.getPreviousAdjustedConsumptions());
  }

  @Test
  public void shouldNotAddPreviousAdjustedConsumptionIfLineSkipped() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);

    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(5);
    previousRequisitionLineItem.setSkipped(true);
    previousRequisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition.setRequisitionLineItems(
        Collections.singletonList(previousRequisitionLineItem));

    Requisition requisition = new Requisition();
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));

    requisition.setPreviousAdjustedConsumptions(
        Collections.singletonList(previousRequisition)
    );

    assertEquals(Collections.emptyList(),
        requisitionLineItem.getPreviousAdjustedConsumptions());
  }

  @Test
  public void shouldNotAddPreviousAdjustedConsumptionIfNull() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);

    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(null);
    previousRequisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition.setRequisitionLineItems(
        Collections.singletonList(previousRequisitionLineItem));

    Requisition requisition = new Requisition();
    requisition.setRequisitionLineItems(Lists.newArrayList(requisitionLineItem));

    requisition.setPreviousAdjustedConsumptions(
        Collections.singletonList(previousRequisition)
    );

    assertEquals(Collections.emptyList(),
        requisitionLineItem.getPreviousAdjustedConsumptions());
  }

  private void prepareForTestAdjustedConcumptionTotalCostAndAverageConsumption() {
    requisitionLineItem = new RequisitionLineItem();
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));

    PowerMockito.spy(LineItemFieldsCalculator.class);
    when(LineItemFieldsCalculator
        .calculateAdjustedConsumption(requisitionLineItem, MONTHS_IN_PERIOD)
    ).thenReturn(ADJUSTED_CONSUMPTION);
    when(LineItemFieldsCalculator.calculateTotalCost(requisitionLineItem))
        .thenReturn(TOTAL_COST);
    when(LineItemFieldsCalculator
        .calculateAverageConsumption(Collections.singletonList(ADJUSTED_CONSUMPTION)))
        .thenReturn(AVERAGE_CONSUMPTION);

    when(template.isColumnInTemplate(any())).thenReturn(true);
    requisition.setTemplate(template);
  }

  private void prepareForTestAverageConsumption(
      List<Integer> adjustedConsumptions) {
    UUID orderableProductId = UUID.randomUUID();
    requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(orderableProductId);
    requisitionLineItem.setPreviousAdjustedConsumptions(adjustedConsumptions);
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));

    PowerMockito.spy(LineItemFieldsCalculator.class);
    when(LineItemFieldsCalculator
        .calculateAdjustedConsumption(requisitionLineItem, MONTHS_IN_PERIOD)
    ).thenReturn(ADJUSTED_CONSUMPTION);

    when(template.isColumnInTemplate(RequisitionLineItem.ADJUSTED_CONSUMPTION)).thenReturn(true);
    when(template.isColumnInTemplate(RequisitionLineItem.AVERAGE_CONSUMPTION)).thenReturn(true);
    requisition.setTemplate(template);
  }

  private void mockReqLine(Requisition requisition, UUID productId,
                           int stockOnHand, int approvedQuantity) {
    RequisitionLineItem item = mock(RequisitionLineItem.class);
    when(item.getOrderableProductId()).thenReturn(productId);
    when(item.getStockOnHand()).thenReturn(stockOnHand);
    when(item.getApprovedQuantity()).thenReturn(approvedQuantity);

    when(requisition.findLineByProductId(productId)).thenReturn(item);
  }

  private ApprovedProductDto mockApprovedProduct(UUID orderableProductId) {
    ApprovedProductDto approvedProductDto = mock(ApprovedProductDto.class);
    ProductDto programProduct = mock(ProductDto.class);
    when(approvedProductDto.getProduct()).thenReturn(programProduct);
    when(programProduct.getProductId()).thenReturn(orderableProductId);
    return approvedProductDto;
  }

  private RequisitionLineItem getRequisitionLineItem(boolean skipped) {
    RequisitionLineItem notSkipped = new RequisitionLineItem();
    notSkipped.setSkipped(skipped);
    notSkipped.setId(UUID.randomUUID());
    return notSkipped;
  }

  private Requisition getRequisition(RequisitionLineItem notSkipped, RequisitionLineItem skipped) {
    Requisition requisition = new Requisition();
    requisition.setRequisitionLineItems(Arrays.asList(notSkipped, skipped));
    return requisition;
  }
}
