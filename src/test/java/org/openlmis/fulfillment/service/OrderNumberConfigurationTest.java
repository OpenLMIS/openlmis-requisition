package org.openlmis.fulfillment.service;


import org.junit.Test;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.referencedata.domain.Program;

import java.util.UUID;

import static org.junit.Assert.assertEquals;

public class OrderNumberConfigurationTest {

  private static final String UUID_STRING = "5625602e-6f5d-11e6-8b77-86f30ca893d3";
  private static final String PROGRAM_CODE = "code";
  private static final String PREFIX = "prefix";
  private static final String EMERGENCY = "E";

  @Test
  public void shouldGenerateOrderNumber() {
    UUID uuid = UUID.fromString(UUID_STRING);
    Program program = new Program();
    program.setCode(PROGRAM_CODE);

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(PREFIX, true, true, true);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(uuid, program.getCode(), true);

    StringBuilder stringBuilder = new StringBuilder(PREFIX);
    stringBuilder.append(PROGRAM_CODE)
        .append(UUID_STRING)
        .append(EMERGENCY);
    String expectedResult = stringBuilder.toString();
    assertEquals(expectedResult, generatedNumber);
  }

  @Test
  public void shouldGenerateOrderNumberWithOnlyRequiredData() {
    UUID uuid = UUID.fromString(UUID_STRING);

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(PREFIX, false, false, false);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(uuid, null, false);

    String expectedResult = UUID_STRING;

    assertEquals(expectedResult, generatedNumber);
  }

  @Test
  public void shouldCorrectlyHandleNullPrefix() {
    UUID uuid = UUID.fromString(UUID_STRING);

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(null, true, false, false);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(uuid, null, true);

    String expectedResult = UUID_STRING;

    assertEquals(expectedResult, generatedNumber);
  }


}
