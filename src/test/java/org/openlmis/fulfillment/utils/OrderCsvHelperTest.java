package org.openlmis.fulfillment.utils;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderFileColumn;
import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;

import java.io.IOException;
import java.io.StringWriter;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class OrderCsvHelperTest {

  private static final String ORDER = "order";
  private static final String LINE_ITEM = "lineItem";

  private static final String ORDER_NUMBER = "Order number";
  private static final String PRODUCT_CODE = "Product code";
  private static final String APPROVED_QUANTITY = "Approved quantity";
  private static final String PERIOD = "Period";
  private static final String ORDER_DATE = "Order date";

  private Order order;

  @Before
  public void setUp() {
    order = createOrder();
  }

  @Test
  public void shouldIncludeHeadersIfRequired() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "", ORDER_NUMBER, true, 1, null,
        ORDER, "orderCode", null));
    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", true, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith(ORDER_NUMBER));

    orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    csv = writeCsvFile(order, orderFileTemplate);
    assertFalse(csv.startsWith(ORDER_NUMBER));
  }

  @Test
  public void shouldExportOrderFields() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.order.number", ORDER_NUMBER,
        true, 1, null, ORDER, "orderCode", null));
    orderFileColumns.add(new OrderFileColumn(true, "label.period", PERIOD,
        true, 2, null, ORDER, "requisition/processingPeriod", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith("code," + order.getRequisition().getProcessingPeriod()));
  }

  @Test
  public void shouldExportRequisitionLineFields() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.product.code", PRODUCT_CODE,
        true, 1, null, LINE_ITEM, "orderableProduct", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.quantity.approved", APPROVED_QUANTITY,
        true, 3, null, LINE_ITEM, "approvedQuantity", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith(order.getRequisition()
        .getRequisitionLines().get(0).getOrderableProduct()
        + ",1"));
  }

  @Test
  public void shouldExportOnlyIncludedColumns() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.order.number", ORDER_NUMBER,
        true, 1, null, ORDER, "orderCode", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.orderableProduct.code", PRODUCT_CODE,
        true, 2, null, LINE_ITEM, "orderableProduct", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.approved.quantity", APPROVED_QUANTITY,
        false, 4, null, LINE_ITEM, "approvedQuantity", null));
    orderFileColumns.add(new OrderFileColumn(true, "label.period", PERIOD,
        true, 5, "MM/yy", ORDER, "requisition/processingPeriod", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        false, 6, "dd/MM/yy", ORDER, "createdDate", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", true, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith(ORDER_NUMBER + "," + PRODUCT_CODE + "," + PERIOD));
  }

  @Test
  public void shouldFormatDates() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        true, 1, "MM/yy", ORDER, "createdDate", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        true, 2, "dd/MM/yy", ORDER, "createdDate", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith("01/16,01/01/16"));
  }

  private String writeCsvFile(Order order, OrderFileTemplate orderFileTemplate)
      throws IOException {
    StringWriter writer = new StringWriter();

    OrderCsvHelper helper = new OrderCsvHelper();
    helper.writeCsvFile(order, orderFileTemplate, writer);

    return writer.toString();
  }

  private Order createOrder() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setOrderableProduct(UUID.randomUUID());
    requisitionLine.setApprovedQuantity(1);

    Requisition requisition = new Requisition();
    requisition.setRequisitionLines(Collections.singletonList(requisitionLine));
    requisition.setProcessingPeriod(UUID.randomUUID());

    Order order = new Order();
    order.setOrderCode("code");
    order.setCreatedDate(LocalDateTime.of(2016, Month.JANUARY, 1, 0, 0));
    order.setRequisition(requisition);

    return order;
  }
}
