package org.openlmis.requisition.domain;

import com.google.common.collect.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static java.util.Arrays.asList;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.verifyStatic;

@PrepareForTest({LineItemFieldsCalculator.class})
@RunWith(PowerMockRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTest {

  private static final Money PRICE_PER_PACK = new Money("9");
  private static final long PACK_SIZE = 2;
  private static final int ADJUSTED_CONSUMPTION = 1;
  private static final int MONTHS_IN_PERIOD = 1;

  private Requisition requisition;
  private RequisitionLineItem requisitionLineItem;
  private OrderableProductDto orderableProductDto;
  private ProcessingPeriodDto periodDto;

  private UUID productId = UUID.randomUUID();

  @Mock
  private RequisitionTemplate template;

  @Before
  public void setUp() {
    orderableProductDto = new OrderableProductDto();
    orderableProductDto.setId(productId);
    orderableProductDto.setPackSize(PACK_SIZE);

    periodDto = new ProcessingPeriodDto();
    periodDto.setStartDate(LocalDate.of(2016, 11, 1));
    periodDto.setEndDate(LocalDate.of(2016, 11, 30));

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
  }

  @Test
  public void shouldAuthorizeRequisitionIfItStatusIsSubmitted() throws RequisitionException {
    Collection<OrderableProductDto> orderableProducts =
        Collections.singletonList(orderableProductDto);
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.authorize(orderableProducts, periodDto);

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingRequisitionWithNotSubmittedStatus()
      throws RequisitionException {
    Collection<OrderableProductDto> orderableProducts =
        Collections.singletonList(orderableProductDto);
    requisition.authorize(orderableProducts, periodDto);
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

    Collection<OrderableProductDto> orderableProducts =
        Collections.singletonList(orderableProductDto);
    requisition.setTemplate(requisitionTemplate);
    requisition.setStatus(RequisitionStatus.SUBMITTED);

    requisition.authorize(orderableProducts, periodDto);
    requisition.updateFrom(new Requisition(), Lists.newArrayList());

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
  public void shouldInitiateRequisitionLineItemFieldsIfPreviousRequisitionProvided()
      throws RequisitionTemplateColumnException {
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
    req.initiate(template, asList(product1, product2), previousRequisition);

    // then
    List<RequisitionLineItem> lineItems = req.getRequisitionLineItems();

    assertEquals(2, lineItems.size());
    assertThat(req.findLineByProductId(productId1).getBeginningBalance(), is(30));
    assertThat(req.findLineByProductId(productId2).getBeginningBalance(), is(33));
  }

  @Test
  public void shouldInitiateBeginningBalanceToZeroIfNoPreviousRequisition()
      throws RequisitionTemplateColumnException {
    // given
    final UUID productId1 = UUID.randomUUID();
    final UUID productId2 = UUID.randomUUID();

    ApprovedProductDto product1 = mockApprovedProduct(productId1);
    ApprovedProductDto product2 = mockApprovedProduct(productId2);

    when(template.isColumnDisplayed(RequisitionLineItem.BEGINNING_BALANCE)).thenReturn(true);

    // when
    Requisition req = new Requisition();
    req.initiate(template, asList(product1, product2), null);

    // then
    List<RequisitionLineItem> lineItems = req.getRequisitionLineItems();

    assertEquals(2, lineItems.size());
    assertThat(req.findLineByProductId(productId1).getBeginningBalance(), is(0));
    assertThat(req.findLineByProductId(productId2).getBeginningBalance(), is(0));
  }

  @Test
  public void shouldInitiateBeginningBalanceToZeroIfNotVisible()
      throws RequisitionTemplateColumnException {
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
    req.initiate(template, asList(product1, product2), previousRequisition);

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
  public void shouldCalculatePacksToShip() {
    List<RequisitionLineItem> items = requisition.getRequisitionLineItems();
    RequisitionLineItem requisitionLineItem = items.get(0);

    assertNull(requisitionLineItem.getPacksToShip());
    assertNull(requisitionLineItem.getTotalCost());

    RequisitionLineItem requisitionLineItem2 = new RequisitionLineItem();

    OrderableProductDto orderableProductDto2 = new OrderableProductDto();
    orderableProductDto2.setId(UUID.randomUUID());
    orderableProductDto2.setPackSize(20);

    requisitionLineItem2.setOrderableProductId(orderableProductDto2.getId());
    requisitionLineItem2.setId(UUID.randomUUID());
    requisitionLineItem2.setRequestedQuantity(20);
    requisitionLineItem2.setStockOnHand(5);
    requisitionLineItem2.setPricePerPack(new Money("25"));
    requisitionLineItem2.setRequisition(requisition);

    assertNull(requisitionLineItem2.getPacksToShip());
    assertNull(requisitionLineItem2.getTotalCost());

    items.add(requisitionLineItem2);
    requisition.setRequisitionLineItems(items);

    Collection<OrderableProductDto> orderableProducts = Arrays.asList(
        orderableProductDto, orderableProductDto2);
    requisition.calculatePacksToShip(orderableProducts);

    assertEquals(5, requisitionLineItem.getPacksToShip().longValue());
    assertEquals(new Money("45"), requisitionLineItem.getTotalCost());

    assertEquals(1, requisitionLineItem2.getPacksToShip().longValue());
    assertEquals(new Money("25"), requisitionLineItem2.getTotalCost());
  }

  @Test
  public void shouldCalculateAdjustedConsumptionWhenSubmit()
      throws RequisitionTemplateColumnException, RequisitionException {
    // given
    Collection<OrderableProductDto> orderableProducts = prepareForTestAdjustedConcumption();
    requisition.setTemplate(mock(RequisitionTemplate.class));

    //when
    requisition.submit(orderableProducts, periodDto);

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
  }

  @Test
  public void shouldCalculateAdjustedConsumptionWhenAuthorize()
      throws RequisitionTemplateColumnException, RequisitionException {
    // given
    Collection<OrderableProductDto> orderableProducts = prepareForTestAdjustedConcumption();
    requisition.setStatus(RequisitionStatus.SUBMITTED);

    //when
    requisition.authorize(orderableProducts, periodDto);

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
  }

  @Test
  public void shouldCalculateAdjustedConsumptionWhenApprove()
      throws RequisitionTemplateColumnException, RequisitionException {
    // given
    Collection<OrderableProductDto> orderableProducts = prepareForTestAdjustedConcumption();
    requisition.setStatus(RequisitionStatus.APPROVED);

    //when
    requisition.approve(orderableProducts, periodDto);

    //then
    assertEquals(ADJUSTED_CONSUMPTION, requisitionLineItem.getAdjustedConsumption().longValue());
  }

  private Collection<OrderableProductDto> prepareForTestAdjustedConcumption() {
    requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(productId);
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));

    mockStatic(LineItemFieldsCalculator.class);
    when(LineItemFieldsCalculator.calculateAdjustedConsumption(any(), eq(MONTHS_IN_PERIOD)))
        .thenReturn(ADJUSTED_CONSUMPTION);

    return Collections.singletonList(orderableProductDto);
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
