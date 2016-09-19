package org.openlmis.fulfillment.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderFileColumn;
import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;

import java.io.IOException;
import java.io.StringWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class OrderCsvHelperTest {

  private static final String ORDER = "order";
  private static final String LINE_ITEM = "lineItem";

  private static final String ORDER_NUMBER = "Order number";
  private static final String PRODUCT_CODE = "Product code";
  private static final String PRODUCT_NAME = "Product name";
  private static final String APPROVED_QUANTITY = "Approved quantity";
  private static final String PERIOD = "Period";
  private static final String ORDER_DATE = "Order date";

  @Test
  public void shouldIncludeHeadersIfRequired() throws IOException {
    Order order = createOrder();

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
        true, 2, null, ORDER, "requisition/processingPeriod/name", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(createOrder(), orderFileTemplate);
    assertTrue(csv.startsWith("code,periodName"));
  }

  @Test
  public void shouldExportRequisitionLineFields() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.product.code", PRODUCT_CODE,
        true, 1, null, LINE_ITEM, "product/code", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.product.name", PRODUCT_NAME,
        true, 2, null, LINE_ITEM, "product/primaryName", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.quantity.approved", APPROVED_QUANTITY,
        true, 3, null, LINE_ITEM, "approvedQuantity", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(createOrder(), orderFileTemplate);
    assertTrue(csv.startsWith("productCode,productName,1"));
  }

  @Test
  public void shouldExportOnlyIncludedColumns() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.order.number", ORDER_NUMBER,
        true, 1, null, ORDER, "orderCode", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.product.code", PRODUCT_CODE,
        false, 2, null, LINE_ITEM, "product/code", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.product.name", PRODUCT_NAME,
        true, 3, null, LINE_ITEM, "product/primaryName", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.quantity.approved", APPROVED_QUANTITY,
        false, 4, null, LINE_ITEM, "approvedQuantity", null));
    orderFileColumns.add(new OrderFileColumn(true, "label.period", PERIOD,
        true, 5, "MM/yy", ORDER, "requisition/processingPeriod/startDate", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        false, 6, "dd/MM/yy", ORDER, "createdDate", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", true, orderFileColumns);

    String csv = writeCsvFile(createOrder(), orderFileTemplate);
    assertTrue(csv.startsWith(ORDER_NUMBER + "," + PRODUCT_NAME + "," + PERIOD));
  }

  @Test
  public void shouldFormatDates() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "label.period", PERIOD,
        true, 1, "MM/yy", ORDER, "requisition/processingPeriod/startDate", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        true, 2, "dd/MM/yy", ORDER, "createdDate", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(createOrder(), orderFileTemplate);
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
    Product product = new Product();
    product.setCode("productCode");
    product.setPrimaryName("productName");

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setApprovedQuantity(1);

    ProcessingPeriod period = new ProcessingPeriod();
    period.setName("periodName");
    period.setStartDate(LocalDate.of(2016, Month.JANUARY, 1));

    Requisition requisition = new Requisition();
    requisition.setRequisitionLines(Collections.singletonList(requisitionLine));
    requisition.setProcessingPeriod(period);

    Order order = new Order();
    order.setOrderCode("code");
    order.setCreatedDate(LocalDateTime.of(2016, Month.JANUARY, 1, 0, 0));
    order.setRequisition(requisition);

    return order;
  }
}
