package org.openlmis.fulfillment.service;


import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.exception.OrderNumberException;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ProgramDto;

import java.util.UUID;

import static org.junit.Assert.assertEquals;

public class OrderNumberConfigurationTest {

  private static final String UUID_STRING = "5625602e-6f5d-11e6-8b77-86f30ca893d3";
  private static final String PROGRAM_CODE = "code";
  private static final String PREFIX = "prefix";
  private static final String EMERGENCY = "E";
  private static final String NOT_EMERGENCY = "R";


  private Requisition requisition;
  private ProgramDto program;

  @Before
  public void setUp() {
    requisition = new Requisition();
    requisition.setId(UUID.fromString(UUID_STRING));
    requisition.setEmergency(true);

    program = new ProgramDto();
    program.setCode(PROGRAM_CODE);
  }

  @Test
  public void shouldGenerateOrderNumber() {

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(PREFIX, true, true, true);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(requisition, program);

    StringBuilder stringBuilder = new StringBuilder(PREFIX);
    stringBuilder.append(PROGRAM_CODE)
        .append(UUID_STRING)
        .append(EMERGENCY);
    String expectedResult = stringBuilder.toString();
    assertEquals(expectedResult, generatedNumber);
  }

  @Test
  public void shouldGenerateOrderNumberWithOnlyRequiredData() {

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(PREFIX, false, false, false);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(requisition, null);

    String expectedResult = UUID_STRING;

    assertEquals(expectedResult, generatedNumber);
  }

  @Test
  public void shouldCorrectlyHandleNullPrefix() {

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(null, true, false, false);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(requisition, program);

    String expectedResult = UUID_STRING;

    assertEquals(expectedResult, generatedNumber);
  }

  @Test
  public void shouldGenerateCorrectSuffixForNotEmergencyRequisition() {

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(null, true, false, true);
    requisition.setEmergency(false);
    String generatedNumber =
        orderNumberConfiguration.generateOrderNumber(requisition, program);

    String expectedResult = UUID_STRING + NOT_EMERGENCY;

    assertEquals(expectedResult, generatedNumber);
  }

  @Test(expected = OrderNumberException.class)
  public void shouldThrowExceptionWhenGeneratingNumberFromNullRequisition() {

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(PREFIX, true, false, false);

    orderNumberConfiguration.generateOrderNumber(null, program);
  }
}
