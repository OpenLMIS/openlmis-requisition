package org.openlmis.fulfillment.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderFileColumn;
import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;

import java.io.IOException;
import java.io.StringWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@SuppressWarnings({"PMD.TooManyMethods"})
@RunWith(MockitoJUnitRunner.class)
public class OrderCsvHelperTest {

  private static final String ORDER = "order";
  private static final String LINE_ITEM = "lineItem";
  private static final String ORDERABLE_PRODUCT = "orderableProductId";

  private static final String ORDER_NUMBER = "Order number";
  private static final String PRODUCT_CODE = "Product code";
  private static final String APPROVED_QUANTITY = "Approved quantity";
  private static final String PERIOD = "Period";
  private static final String ORDER_DATE = "Order date";

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  @InjectMocks
  private OrderCsvHelper orderCsvHelper;

  private Order order;

  @Before
  public void setUp() {
    order = createOrder();

    UUID facilityId = order.getRequisition().getFacilityId();
    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(createFacility());

    UUID periodId = order.getRequisition().getProcessingPeriodId();
    when(periodReferenceDataService.findOne(periodId)).thenReturn(createPeriod());

    UUID productId = order.getRequisition().getRequisitionLineItems()
        .get(0).getOrderableProductId();
    when(orderableProductReferenceDataService.findOne(productId)).thenReturn(createProduct());
  }

  @Test
  public void shouldIncludeHeadersIfRequired() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "", ORDER_NUMBER, true, 1, null,
        ORDER, "orderCode", null, null, null));
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
        true, 1, null, ORDER, "orderCode", null, null, null));
    orderFileColumns.add(new OrderFileColumn(true, "header.status", "Status",
        true, 2, null, ORDER, "requisition/status", null, null, null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith("code," + RequisitionStatus.SUBMITTED));
  }

  @Test
  public void shouldExportRequisitionLineItemFields() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.orderableProduct", "Product",
        true, 1, null, LINE_ITEM, ORDERABLE_PRODUCT, null, null, null));
    orderFileColumns.add(new OrderFileColumn(true, "header.quantity.approved", APPROVED_QUANTITY,
        true, 2, null, LINE_ITEM, "approvedQuantity", null, null, null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);

    assertTrue(csv.startsWith(order.getRequisition()
        .getRequisitionLineItems().get(0).getOrderableProductId()
        + ",1"));
  }

  @Test
  public void shouldExportOnlyIncludedColumns() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.order.number", ORDER_NUMBER,
        true, 1, null, ORDER, "orderCode", null, null, null));
    orderFileColumns.add(new OrderFileColumn(true, "header.orderableProduct", "Product",
        true, 2, null, LINE_ITEM, ORDERABLE_PRODUCT, null, null, null));
    orderFileColumns.add(new OrderFileColumn(true, "header.approved.quantity", APPROVED_QUANTITY,
        false, 3, null, LINE_ITEM, "approvedQuantity", null, null, null));
    orderFileColumns.add(new OrderFileColumn(true, "header.status", "Status",
        true, 4, "MM/yy", ORDER, "requisition/status", null, null, null));
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        false, 5, "dd/MM/yy", ORDER, "createdDate", null, null, null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", true, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith(ORDER_NUMBER + ",Product,Status"));
  }

  @Test
  public void shouldExportRelatedFields() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.facility.code", "Facility code",
        true, 1, null, ORDER, "requisition/facilityId", "Facility", "code", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.product.code", PRODUCT_CODE,
        true, 2, null, LINE_ITEM, ORDERABLE_PRODUCT, "OrderableProduct", "productCode", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.product.name", "Product name",
        true, 3, null, LINE_ITEM, ORDERABLE_PRODUCT, "OrderableProduct", "name", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.period", PERIOD, true, 4,
        "MM/yy", ORDER, "requisition/processingPeriodId", "ProcessingPeriod", "startDate", null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith("facilityCode,productCode,productName,01/16"));
  }

  @Test
  public void shouldFormatDates() throws IOException {
    List<OrderFileColumn> orderFileColumns = new ArrayList<>();
    orderFileColumns.add(new OrderFileColumn(true, "header.period", PERIOD, true, 1,
        "MM/yy", ORDER, "requisition/processingPeriodId", "ProcessingPeriod", "startDate", null));
    orderFileColumns.add(new OrderFileColumn(true, "header.order.date", ORDER_DATE,
        true, 2, "dd/MM/yy", ORDER, "createdDate", null, null, null));

    OrderFileTemplate orderFileTemplate = new OrderFileTemplate("O", false, orderFileColumns);

    String csv = writeCsvFile(order, orderFileTemplate);
    assertTrue(csv.startsWith("01/16,01/01/16"));
  }

  private String writeCsvFile(Order order, OrderFileTemplate orderFileTemplate)
      throws IOException {
    StringWriter writer = new StringWriter();

    orderCsvHelper.writeCsvFile(order, orderFileTemplate, writer);

    return writer.toString();
  }

  private Order createOrder() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(UUID.randomUUID());
    requisitionLineItem.setApprovedQuantity(1);

    Requisition requisition = new Requisition();
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));
    requisition.setProcessingPeriodId(UUID.randomUUID());
    requisition.setFacilityId(UUID.randomUUID());

    Order order = new Order();
    order.setOrderCode("code");
    order.setCreatedDate(LocalDateTime.of(2016, Month.JANUARY, 1, 0, 0));
    order.setRequisition(requisition);

    return order;
  }

  private FacilityDto createFacility() {
    FacilityDto facility = new FacilityDto();
    facility.setCode("facilityCode");

    return facility;
  }

  private ProcessingPeriodDto createPeriod() {
    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setName("periodName");
    period.setStartDate(LocalDate.of(2016, Month.JANUARY, 1));

    return period;
  }

  private OrderableProductDto createProduct() {
    OrderableProductDto product = new OrderableProductDto();
    product.setProductCode("productCode");
    product.setName("productName");

    return product;
  }
}
