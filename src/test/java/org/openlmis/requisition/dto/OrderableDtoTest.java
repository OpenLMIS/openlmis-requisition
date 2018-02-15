/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.dto;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.openlmis.requisition.dto.OrderableDto.COMMODITY_TYPE_IDENTIFIER;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;

import nl.jqno.equalsverifier.EqualsVerifier;

import org.junit.Before;
import org.junit.Test;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class OrderableDtoTest extends DtoTest {

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

  @Override
  protected Class<OrderableDto> getTestClass() {
    return OrderableDto.class;
  }

  @Override
  protected void prepare(EqualsVerifier verifier) {
    verifier.withRedefinedSuperclass();
  }

  @Test
  public void shouldCalculatePacksToOrderWhenPackRoundingThresholdIsSmallerThanRemainder() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(10);
    productDto.setPackRoundingThreshold(4);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(26);

    assertEquals(3, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenPackRoundingThresholdIsGreaterThanRemainder() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(26);

    assertEquals(2, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenCanRoundToZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);

    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldCalculatePacksToOrderWhenCanNotRoundToZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(1, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPacksToOrderIfNetContentIsZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(0);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);
    long packsToOrder = productDto.packsToOrder(6);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPacksToOrderIfOrderQuantityIsZero() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(10);
    productDto.setRoundToZero(false);

    long packsToOrder = productDto.packsToOrder(0);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldReturnZeroPackToOrderIfOrderQuantityIsOneAndRoundToZeroTrueWithNetContentTen() {
    OrderableDto productDto = new OrderableDto();
    productDto.setNetContent(10);
    productDto.setPackRoundingThreshold(7);
    productDto.setRoundToZero(true);

    long packsToOrder = productDto.packsToOrder(1);

    assertEquals(0, packsToOrder);
  }

  @Test
  public void shouldNotRoundUpWhenEqualToThreshold() {
    OrderableDto product = new OrderableDto();
    product.setNetContent(100);
    product.setPackRoundingThreshold(50);

    long packsToOrder = product.packsToOrder(250);
    assertEquals(2, packsToOrder);

    packsToOrder = product.packsToOrder(251);
    assertEquals(3, packsToOrder);
  }

  @Test
  public void shouldFindProgramOrderable() {
    ProgramOrderableDto programOrderableDto = new ProgramOrderableDto();
    programOrderableDto.setProgramId(program.getId());
    products.add(programOrderableDto);
    orderableDto.setPrograms(products);

    ProgramOrderableDto programOrderableDtoAfterFind =
        orderableDto.findProgramOrderableDto(program.getId());

    assertEquals(programOrderableDtoAfterFind, programOrderableDto);
    assertEquals(programOrderableDtoAfterFind.getProgramId(), programOrderableDto.getProgramId());
  }

  @Test
  public void shouldReturnCommodityTypeIdentifier() {
    String value = UUID.randomUUID().toString();
    orderableDto.setIdentifiers(ImmutableMap.of(COMMODITY_TYPE_IDENTIFIER, value));
    assertThat(orderableDto.getCommodityTypeIdentifier(), is(value));
  }

  @Test
  public void shouldReturnNullIfCommodityTypeIdentifierNotExist() {
    // null as map
    orderableDto.setIdentifiers(null);
    assertThat(orderableDto.getCommodityTypeIdentifier(), is(nullValue()));

    // empty map
    Map<String,String> identifiers = Maps.newHashMap();

    orderableDto.setIdentifiers(identifiers);
    assertThat(orderableDto.getCommodityTypeIdentifier(), is(nullValue()));

    // null value
    identifiers.put(COMMODITY_TYPE_IDENTIFIER, null);
    assertThat(orderableDto.getCommodityTypeIdentifier(), is(nullValue()));

    // white spaces value
    identifiers.put(COMMODITY_TYPE_IDENTIFIER, "          ");
    assertThat(orderableDto.getCommodityTypeIdentifier(), is(nullValue()));

    // empty ("") value
    identifiers.put(COMMODITY_TYPE_IDENTIFIER, "");
    assertThat(orderableDto.getCommodityTypeIdentifier(), is(nullValue()));
  }

  private Set<ProgramOrderableDto> genereteProducts(int instances) {
    Set<ProgramOrderableDto> programs = new HashSet<>();
    for (int i = 0; i < instances; i++) {
      ProgramDto program = new ProgramDto();
      program.setId(UUID.randomUUID());
      OrderableDto orderableDto = new OrderableDto();
      orderableDto.setId(UUID.randomUUID());
      ProgramOrderableDto programOrderableDto = new ProgramOrderableDto();
      programOrderableDto.setProgramId(program.getId());
      programs.add(programOrderableDto);
    }
    return programs;
  }
}
