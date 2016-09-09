package org.openlmis.csv.generator;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.service.OrderService;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.dto.UserDto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class CsvGeneratorTest {

  private Facility facility = new Facility();
  private Program program = new Program();
  private UserDto user = new UserDto();

  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("10");

    GeographicLevel level = new GeographicLevel();
    level.setCode("20");
    level.setLevelNumber(1);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("1u1u1u1");
    geographicZone.setLevel(level);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("Example fCode");
    facility.setName("Example fName");
    facility.setDescription("Example fDescr");
    facility.setActive(true);
    facility.setEnabled(true);
    facility.setId(new UUID(50,60));

    program.setCode("1h1h1h1");

    user.setUsername("Name, Surname");
    user.setEmail("mail@mail.com");
    user.setFirstName("Klasa");
    user.setLastName("User");
    user.setId(new UUID(50,50));
  }

  @Test
  public void testCsvWrite() {
    List<OrderLine> orderLines = generateOrderLines();
    Order testOrder = Mockito.spy(generateInstance());
    Mockito.doReturn(orderLines).when(testOrder).getOrderLines();

    List<String> header = new ArrayList<>();
    header.add(OrderService.DEFAULT_COLUMNS[0]);
    header.add(OrderService.DEFAULT_COLUMNS[1]);
    header.add(OrderService.DEFAULT_COLUMNS[3]);
    header.add(OrderService.DEFAULT_COLUMNS[4]);
    header.add(OrderService.DEFAULT_COLUMNS[5]);

    OrderService generator = new OrderService();
    String csv = generator.orderToCsv(testOrder, header.toArray(new String[0]));

    String exp = "facilityCode,createdDate,productName,productCode,orderedQuantity\r\n"
            + "Example fCode,1410-07-15T10:11:30,Example pName,1Q1Q1Q1,11111111\r\n"
            + "Example fCode,1410-07-15T10:11:30,Example pName 2,2Q2Q2Q2,22222222\r\n"
            + "Example fCode,1410-07-15T10:11:30,Example pName 3,3Q3Q3Q3,33333333\r\n";
    Assert.assertEquals(exp, csv);
  }

  private Order generateInstance() {
    Order order = new Order();
    order.setOrderCode("1t1t1t");
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    LocalDateTime date = LocalDateTime.parse("1410-07-15T10:11:30");
    order.setCreatedDate(date);

    return order;
  }

  private List<OrderLine> generateOrderLines() {
    List<OrderLine> orderLines = new ArrayList<>();
    orderLines.add(generateOrderLine("Example pName", "1Q1Q1Q1", 11111111));
    orderLines.add(generateOrderLine("Example pName 2", "2Q2Q2Q2", 22222222));
    orderLines.add(generateOrderLine("Example pName 3", "3Q3Q3Q3", 33333333));

    return orderLines;
  }

  private OrderLine generateOrderLine(String prodName, String prodCode, long orderQ) {
    OrderLine example = new OrderLine();
    Product prod = new Product();
    prod.setCode(prodCode);
    prod.setPrimaryName(prodName);
    example.setOrderedQuantity(orderQ);
    example.setProduct(prod);

    return example;
  }
}
