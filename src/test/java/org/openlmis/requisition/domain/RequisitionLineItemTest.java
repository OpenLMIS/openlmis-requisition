package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineItemTest {

  private Requisition initiatedRequisition;

  @Before
  public void setUp() {
    initiatedRequisition = mockReq(RequisitionStatus.INITIATED);
  }

  @Test
  public void shouldOnlyUpdateApprovedFieldsWhenRequisitionStatusIsAuthorized() {
    Requisition requisition = mockReq(RequisitionStatus.AUTHORIZED);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setApprovedQuantity(1);
    requisitionLineItem.setRemarks("Remarks");
    requisitionLineItem.setStockOnHand(5);

    RequisitionLineItem updatedItem = new RequisitionLineItem();
    updatedItem.setRequisition(requisition);
    updatedItem.updateFrom(requisitionLineItem);

    assertEquals(1, updatedItem.getApprovedQuantity().intValue());
    assertEquals("Remarks", updatedItem.getRemarks());
    assertNull(updatedItem.getStockOnHand());
  }

  @Test
  public void shouldUpdateSubmissionFields() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);

    item.setStockOnHand(1);
    item.setTotalConsumedQuantity(2);
    item.setBeginningBalance(3);
    item.setTotalReceivedQuantity(4);
    item.setRequestedQuantity(5);
    item.setTotalStockoutDays(6);
    item.setTotal(7);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setStockOnHand(11);
    updateItem.setTotalConsumedQuantity(22);
    updateItem.setBeginningBalance(33);
    updateItem.setTotalReceivedQuantity(44);
    updateItem.setRequestedQuantity(55);
    updateItem.setTotalStockoutDays(66);
    updateItem.setTotal(77);

    item.updateFrom(updateItem);

    assertThat(item.getStockOnHand(), is(11));
    assertThat(item.getTotalConsumedQuantity(), is(22));
    assertThat(item.getBeginningBalance(), is(33));
    assertThat(item.getTotalReceivedQuantity(), is(44));
    assertThat(item.getRequestedQuantity(), is(55));
    assertThat(item.getTotalStockoutDays(), is(66));
    assertThat(item.getTotal(), is(77));

  }

  @Test
  public void shouldNotUpdateProduct() {
    final UUID product1 = UUID.randomUUID();
    final UUID product2 = UUID.randomUUID();

    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(initiatedRequisition);
    item.setOrderableProductId(product1);

    RequisitionLineItem updateItem = new RequisitionLineItem();
    updateItem.setRequisition(initiatedRequisition);
    updateItem.setOrderableProductId(product2);

    item.updateFrom(updateItem);

    assertThat(item.getOrderableProductId(), is(product1));
  }

  @Test
  public void shouldReturnApprovedQuantityWhenItIsNotNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setApprovedQuantity(4);

    assertEquals(4, item.getOrderQuantity().intValue());
  }

  @Test
  public void shouldReturnRequestedQuantityWhenItIsNotNullAndApprovedQuantityIsNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequestedQuantity(5);
    item.setApprovedQuantity(null);

    assertEquals(5, item.getOrderQuantity().intValue());
  }

  @Test
  public void shouldReturnZeroWhenApprovedQuantityAndRequestedQuantityIsNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setApprovedQuantity(null);
    item.setRequestedQuantity(null);

    assertEquals(0, item.getOrderQuantity().intValue());
  }

  @Test
  public void shouldBuildFromConstructorAndExport() {
    Money pricePerPack = new Money("5.7986");
    RequisitionLineItemDto lineItemDto = testConstructionAndExport(pricePerPack);

    assertThat(lineItemDto.getPricePerPack(), is(pricePerPack));
  }

  @Test
  public void shouldBuildFromConstructorAndExportWithDefaultPrice() {
    RequisitionLineItemDto lineItemDto = testConstructionAndExport(null);

    assertThat(lineItemDto.getPricePerPack(),
            is(new Money(RequisitionLineItem.PRICE_PER_PACK_IF_NULL)));
  }

  @Test
  public void shouldReturnNumberOfNewPatientsAddedWhenItIsNotNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setNumberOfNewPatientsAdded(10);

    assertEquals(10, item.getNumberOfNewPatientsAdded().intValue());
  }

  @Test
  public void shouldReturnZeroWhenNumberOfNewPatientsAddedIsNull() {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setNumberOfNewPatientsAdded(null);

    assertEquals(0, item.getNumberOfNewPatientsAdded().intValue());
  }

  private RequisitionLineItemDto testConstructionAndExport(Money pricePerPack) {
    UUID productId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    ProgramDto program = new ProgramDto();
    program.setId(programId);
    ProductDto programProduct = new ProductDto();
    programProduct.setPricePerPack(pricePerPack);
    programProduct.setProductId(productId);
    ApprovedProductDto ftap = new ApprovedProductDto();
    ftap.setProduct(programProduct);
    when(initiatedRequisition.getProgramId()).thenReturn(programId);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem(initiatedRequisition, ftap);

    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setBeginningBalance(3);
    requisitionLineItem.setTotalReceivedQuantity(4);
    requisitionLineItem.setTotalLossesAndAdjustments(0);
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setRequestedQuantity(5);
    requisitionLineItem.setTotalConsumedQuantity(2);
    requisitionLineItem.setTotal(7);
    requisitionLineItem.setApprovedQuantity(5);
    requisitionLineItem.setTotalStockoutDays(6);
    requisitionLineItem.setRemarks("remarks");
    requisitionLineItem.setRequestedQuantityExplanation("explanation");
    requisitionLineItem.setTotalCost(new Money("30"));

    OrderableProductDto orderableProductDto = generateOrderableProductDto(program, programProduct);

    RequisitionLineItemDto dto = new RequisitionLineItemDto();
    requisitionLineItem.export(dto, orderableProductDto);

    assertNotNull(dto);
    assertThat(dto.getOrderableProduct().getId(), is(requisitionLineItem.getOrderableProductId()));
    assertThat(dto.getId(), is(requisitionLineItem.getId()));
    assertThat(dto.getBeginningBalance(), is(requisitionLineItem.getBeginningBalance()));
    assertThat(dto.getTotalReceivedQuantity(), is(requisitionLineItem.getTotalReceivedQuantity()));
    assertThat(dto.getTotalLossesAndAdjustments(),
            is(requisitionLineItem.getTotalLossesAndAdjustments()));
    assertThat(dto.getStockOnHand(), is(requisitionLineItem.getStockOnHand()));
    assertThat(dto.getRequestedQuantity(), is(requisitionLineItem.getRequestedQuantity()));
    assertThat(dto.getTotalConsumedQuantity(), is(requisitionLineItem.getTotalConsumedQuantity()));
    assertThat(dto.getTotal(), is(requisitionLineItem.getTotal()));
    assertThat(dto.getApprovedQuantity(), is(requisitionLineItem.getApprovedQuantity()));
    assertThat(dto.getTotalStockoutDays(), is(requisitionLineItem.getTotalStockoutDays()));
    assertThat(dto.getRemarks(), is(requisitionLineItem.getRemarks()));
    assertThat(dto.getRequestedQuantityExplanation(),
            is(requisitionLineItem.getRequestedQuantityExplanation()));
    assertThat(dto.getTotalCost(), is(requisitionLineItem.getTotalCost()));

    return dto;
  }

  private OrderableProductDto generateOrderableProductDto(ProgramDto program,
                                                          ProductDto productDto) {
    OrderableProductDto orderableProductDto = new OrderableProductDto();
    orderableProductDto.setId(productDto.getProductId());
    program.setId(UUID.randomUUID());
    productDto.setProductId(orderableProductDto.getId());
    productDto.setProgramId(program.getId());
    Set<ProductDto> programs = new HashSet<>();
    programs.add(productDto);
    orderableProductDto.setPrograms(programs);
    return orderableProductDto;
  }

  private Requisition mockReq(RequisitionStatus status) {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(status);
    return requisition;
  }
}
