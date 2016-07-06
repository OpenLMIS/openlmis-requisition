package org.openlmis.referencedata.utils;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.*;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class CSVGeneratorTest {

  private Facility facility = new Facility();
  private Program program = new Program();
  private User user = new User();

  Order generateInstance() {
    Order order = new Order();
    order.setOrderCode("\"aaa\"");
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    return order;
  }

  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("10");

    GeographicLevel level = new GeographicLevel();
    level.setCode("20");
    level.setLevelNumber(1);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("30");
    geographicZone.setLevel(level);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("40");
    facility.setName("FacilityName");
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);

    program.setCode("50");

    user.setUsername("Name, Surname");
    user.setPassword("Pswd");
    user.setFirstName("Test");
    user.setLastName("User");
  }

  @Test
  public void testCSVWrite() {
    CsvGenerator generator = new CsvGenerator();
    Order testOrder = generateInstance();

    List<String> headlines = new ArrayList<String>();
    headlines.add("U");
    headlines.add("O");
    headlines.add("Q");
    headlines.add("F");

    List<String> fields = new ArrayList<String>();
    fields.add(testOrder.getCreatedBy().getUsername());
    fields.add(testOrder.getOrderCode());
    fields.add(testOrder.getQuotedCost().toString());
    fields.add(testOrder.getReceivingFacility().getName());

    String tmp = generator.appendHeadlines(headlines);
    String csv = generator.appendRecord(fields, tmp);

    String expected = "U,O,Q,F\r\n\"Name, Surname\",\"\"\"aaa\"\"\",1.29,FacilityName\r\n";
    Assert.assertEquals(expected, csv);

    fields.clear();
    fields.add(testOrder.getQuotedCost().toString());
    fields.add(null);
    fields.add(null);
    fields.add(testOrder.getCreatedBy().getUsername());

    csv = generator.appendRecord(fields, tmp);

    expected = "U,O,Q,F\r\n1.29,,,\"Name, Surname\"\r\n";
    Assert.assertEquals(expected, csv);
  }
}
