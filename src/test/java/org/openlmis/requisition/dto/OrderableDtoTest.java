package org.openlmis.requisition.dto;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

public class OrderableDtoTest {

  private OrderableDto orderableDto;
  private Set<ProgramOrderableDto> products;
  private ProgramDto program;

  @Before
  public void setUp() {
    products = new HashSet<>();
    products.addAll(genereteProducts(10));

    program = new ProgramDto();
    program.setId(UUID.randomUUID());
    orderableDto = new OrderableDto();
    orderableDto.setId(UUID.randomUUID());
  }

  @Test
  public void shouldCalculatePacksToOrderWhenPackRoundingThresholdIsSmallerThanRemainder() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(4);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(26);

    assertEquals(3, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenPackRoundingThresholdIsGreaterThanRemainder() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(26);

    assertEquals(2, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenCanRoundToZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);

    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenCanNotRoundToZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(1, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPacksToOrderIfPackSizeIsZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(0);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);
    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPacksToOrderIfOrderQuantityIsZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(10);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(0);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPackToOrderIfOrderQuantityIsOneAndRoundToZeroTrueWithPackSizeTen() {
    OrderableDto productDto = new OrderableDto();
    productDto.setPackSize(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);

    long packsToOrder = productDto.packsToOrder(1);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldNotRoundUpWhenEqualToThreshold() {
    OrderableDto product = new OrderableDto();
    product.setPackSize(100);
    product.setPackRoundingThreshold(50);

    long packsToOrder = product.packsToOrder(250);
    assertEquals(2, packsToOrder);

    packsToOrder = product.packsToOrder(251);
    assertEquals(3, packsToOrder);
  }

  @Test
  public void shouldFindProgramOrderable() {
    ProgramOrderableDto programOrderableDto = new ProgramOrderableDto();
    programOrderableDto.setOrderableId(orderableDto.getId());
    programOrderableDto.setProgramId(program.getId());
    products.add(programOrderableDto);
    orderableDto.setPrograms(products);

    ProgramOrderableDto programOrderableDtoAfterFind =
        orderableDto.findProgramOrderableDto(program.getId());

    assertEquals(programOrderableDtoAfterFind, programOrderableDto);
    assertEquals(programOrderableDtoAfterFind.getProgramId(), programOrderableDto.getProgramId());
    assertEquals(programOrderableDtoAfterFind.getOrderableId(),
        programOrderableDto.getOrderableId());
  }

  private Set<ProgramOrderableDto> genereteProducts(int instances) {
    Set<ProgramOrderableDto> programs = new HashSet<>();
    for (int i = 0; i < instances; i++) {
      ProgramDto program = new ProgramDto();
      program.setId(UUID.randomUUID());
      OrderableDto orderableDto = new OrderableDto();
      orderableDto.setId(UUID.randomUUID());
      ProgramOrderableDto programOrderableDto = new ProgramOrderableDto();
      programOrderableDto.setOrderableId(orderableDto.getId());
      programOrderableDto.setProgramId(program.getId());
      programs.add(programOrderableDto);
    }
    return programs;
  }
}
