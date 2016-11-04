package org.openlmis.fulfillment.utils;

import static java.time.format.DateTimeFormatter.ofPattern;
import static org.apache.commons.collections.CollectionUtils.filter;

import org.apache.commons.collections.Predicate;
import org.apache.commons.jxpath.JXPathContext;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderFileColumn;
import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.Writer;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Component
public class OrderCsvHelper {
  private static final String STRING = "string";
  private static final String LINE_NO = "line_no";
  private static final String ORDER = "order";

  private static final String FACILITY = "Facility";
  private static final String PRODUCT = "OrderableProduct";
  private static final String PERIOD = "ProcessingPeriod";

  private static final String LINE_SEPARATOR = "\r\n";
  private static final Boolean ENCLOSE_VALUES_WITH_QUOTES = false;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  /**
   * Exporting order to csv.
   */
  public void writeCsvFile(Order order, OrderFileTemplate orderFileTemplate, Writer writer)
      throws IOException {
    List<OrderFileColumn> orderFileColumns = orderFileTemplate.getOrderFileColumns();
    removeExcludedColumns(orderFileColumns);
    if (orderFileTemplate.getHeaderInFile()) {
      writeHeader(orderFileColumns, writer);
    }

    writeLineItems(order, order.getRequisition().getRequisitionLineItems(),
        orderFileColumns, writer);
  }

  private void removeExcludedColumns(List<OrderFileColumn> orderFileColumns) {
    filter(orderFileColumns, new Predicate() {
      @Override
      public boolean evaluate(Object object) {
        return ((OrderFileColumn) object).getInclude();
      }
    });
  }

  private void writeHeader(List<OrderFileColumn> orderFileColumns, Writer writer)
      throws IOException {
    for (OrderFileColumn column : orderFileColumns) {
      String columnLabel = column.getColumnLabel();
      if (columnLabel == null) {
        columnLabel = "";
      }
      writer.write(columnLabel);
      if (orderFileColumns.indexOf(column) == (orderFileColumns.size() - 1)) {
        writer.write(LINE_SEPARATOR);
        break;
      }
      writer.write(",");
    }
  }

  private void writeLineItems(Order order, List<RequisitionLineItem> requisitionLineItems,
                              List<OrderFileColumn> orderFileColumns, Writer writer)
      throws IOException {
    int counter = 1;
    for (RequisitionLineItem requisitionLineItem : requisitionLineItems) {
      writeCsvLineItem(order, requisitionLineItem, orderFileColumns, writer, counter++);
      writer.write(LINE_SEPARATOR);
    }
  }

  private void writeCsvLineItem(Order order, RequisitionLineItem requisitionLineItem,
                                List<OrderFileColumn> orderFileColumns, Writer writer, int counter)
      throws IOException {
    JXPathContext orderContext = JXPathContext.newContext(order);
    JXPathContext lineItemContext = JXPathContext.newContext(requisitionLineItem);
    for (OrderFileColumn orderFileColumn : orderFileColumns) {
      if (orderFileColumn.getNested() == null || orderFileColumn.getNested().isEmpty()) {
        if (orderFileColumns.indexOf(orderFileColumn) < orderFileColumns.size() - 1) {
          writer.write(",");
        }
        continue;
      }
      Object columnValue = getColumnValue(counter, orderContext, lineItemContext, orderFileColumn);

      if (columnValue instanceof LocalDateTime) {
        columnValue = ((LocalDateTime) columnValue).format(ofPattern(orderFileColumn.getFormat()));
      } else if (columnValue instanceof LocalDate) {
        columnValue = ((LocalDate) columnValue).format(ofPattern(orderFileColumn.getFormat()));
      }
      if (ENCLOSE_VALUES_WITH_QUOTES) {
        writer.write("\"" + (columnValue).toString() + "\"");
      } else {
        writer.write((columnValue).toString());
      }
      if (orderFileColumns.indexOf(orderFileColumn) < orderFileColumns.size() - 1) {
        writer.write(",");
      }
    }
  }

  private Object getColumnValue(int counter, JXPathContext orderContext,
                                JXPathContext lineItemContext, OrderFileColumn orderFileColumn) {
    Object columnValue;

    switch (orderFileColumn.getNested()) {
      case STRING:
        columnValue = orderFileColumn.getKeyPath();
        break;
      case LINE_NO:
        columnValue = counter;
        break;
      case ORDER:
        columnValue = orderContext.getValue(orderFileColumn.getKeyPath());
        break;
      default:
        columnValue = lineItemContext.getValue(orderFileColumn.getKeyPath());
        break;
    }

    if (orderFileColumn.getRelated() != null && !orderFileColumn.getRelated().isEmpty()) {
      columnValue = getRelatedColumnValue((UUID) columnValue, orderFileColumn);
    }

    return columnValue == null ? "" : columnValue;
  }

  private Object getRelatedColumnValue(UUID relatedId, OrderFileColumn orderFileColumn) {
    if (relatedId == null) {
      return null;
    }

    Object columnValue;

    switch (orderFileColumn.getRelated()) {
      case FACILITY:
        FacilityDto facility = facilityReferenceDataService.findOne(relatedId);
        columnValue = getValue(facility, orderFileColumn.getRelatedKeyPath());
        break;
      case PRODUCT:
        OrderableProductDto product = orderableProductReferenceDataService.findOne(relatedId);
        columnValue = getValue(product, orderFileColumn.getRelatedKeyPath());
        break;
      case PERIOD:
        ProcessingPeriodDto period = periodService.getPeriod(relatedId);
        columnValue = getValue(period, orderFileColumn.getRelatedKeyPath());
        break;
      default:
        columnValue = null;
        break;
    }

    return columnValue;
  }

  private Object getValue(Object object, String keyPath) {
    JXPathContext context = JXPathContext.newContext(object);

    return context.getValue(keyPath);
  }
}
