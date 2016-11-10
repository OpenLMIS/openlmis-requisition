package org.openlmis.requisition.dto;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

public class OrderableProductDtoTest {

  OrderableProductDto orderableProductDto;
  Set<ProgramProductDto> programs;
  ProgramDto program;

  @Before
  public void setUp() {
    programs = new HashSet<>();
    programs.addAll(genereteProgram(10));

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
  public void shouldFindProgramProduct() {
    ProgramProductDto programProductDto = new ProgramProductDto();
    programProductDto.setProductId(orderableProductDto.getId());
    programProductDto.setProgramId(program.getId());
    programs.add(programProductDto);
    orderableProductDto.setPrograms(programs);

    ProgramProductDto programProductDtoAfterFind =
        orderableProductDto.findProgramProductDto(program.getId());

    assertEquals(programProductDtoAfterFind, programProductDto);
    assertEquals(programProductDtoAfterFind.getProgramId(), programProductDto.getProgramId());
    assertEquals(programProductDtoAfterFind.getProductId(), programProductDto.getProductId());
  }

  private Set<ProgramProductDto> genereteProgram(int instances) {
    Set<ProgramProductDto> programs = new HashSet<>();
    for (int i = 0; i < instances; i++) {
      ProgramDto program = new ProgramDto();
      program.setId(UUID.randomUUID());
      OrderableProductDto orderableProductDto = new OrderableProductDto();
      orderableProductDto.setId(UUID.randomUUID());
      ProgramProductDto programProductDto = new ProgramProductDto();
      programProductDto.setProductId(orderableProductDto.getId());
      programProductDto.setProgramId(program.getId());
      programs.add(programProductDto);
    }
    return programs;
  }
}
