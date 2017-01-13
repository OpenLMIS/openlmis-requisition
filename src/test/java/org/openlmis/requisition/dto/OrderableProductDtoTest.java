package org.openlmis.requisition.dto;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

public class OrderableProductDtoTest {

  private OrderableProductDto orderableProductDto;
  private Set<ProductDto> products;
  private ProgramDto program;

  @Before
  public void setUp() {
    products = new HashSet<>();
    products.addAll(genereteProducts(10));

    program = new ProgramDto();
    program.setId(UUID.randomUUID());
    orderableProductDto = new OrderableProductDto();
    orderableProductDto.setId(UUID.randomUUID());
  }

  @Test
  public void shouldCalculatePacksToOrderWhenPackRoundingThresholdIsSmallerThanRemainder() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(4);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(26);

    assertEquals(3, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenPackRoundingThresholdIsGreaterThanRemainder() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(26);

    assertEquals(2, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenCanRoundToZero() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);

    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenCanNotRoundToZero() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(1, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPacksToOrderIfPackSizeIsZero() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(0);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);
    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPacksToOrderIfOrderQuantityIsZero() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(10);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(0);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPackToOrderIfOrderQuantityIsOneAndRoundToZeroTrueWithPackSizeTen() {
    OrderableProductDto productDto = new OrderableProductDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);

    long packsToOrder = productDto.packsToOrder(1);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldNotRoundUpWhenEqualToThreshold() {
    OrderableProductDto product = new OrderableProductDto();
    product.setPackSize(100);
    product.setPackRoundingThreshold(50);

    long packsToOrder = product.packsToOrder(250);
    assertEquals(2, packsToOrder);

    packsToOrder = product.packsToOrder(251);
    assertEquals(3, packsToOrder);
  }

  @Test
  public void shouldFindProgramProduct() {
    ProductDto productDto = new ProductDto();
    productDto.setProductId(orderableProductDto.getId());
    productDto.setProgramId(program.getId());
    products.add(productDto);
    orderableProductDto.setPrograms(products);

    ProductDto productDtoAfterFind =
        orderableProductDto.findProgramProductDto(program.getId());

    assertEquals(productDtoAfterFind, productDto);
    assertEquals(productDtoAfterFind.getProgramId(), productDto.getProgramId());
    assertEquals(productDtoAfterFind.getProductId(), productDto.getProductId());
  }

  private Set<ProductDto> genereteProducts(int instances) {
    Set<ProductDto> programs = new HashSet<>();
    for (int i = 0; i < instances; i++) {
      ProgramDto program = new ProgramDto();
      program.setId(UUID.randomUUID());
      OrderableProductDto orderableProductDto = new OrderableProductDto();
      orderableProductDto.setId(UUID.randomUUID());
      ProductDto productDto = new ProductDto();
      productDto.setProductId(orderableProductDto.getId());
      productDto.setProgramId(program.getId());
      programs.add(productDto);
    }
    return programs;
  }
}
