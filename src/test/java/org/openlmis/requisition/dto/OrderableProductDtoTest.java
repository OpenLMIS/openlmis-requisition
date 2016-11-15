package org.openlmis.requisition.dto;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class OrderableProductDtoTest {

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

}
