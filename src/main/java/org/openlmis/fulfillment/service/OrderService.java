package org.openlmis.fulfillment.service;

import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
import org.openlmis.csv.generator.CsvGenerator;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.OutputStream;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

@Service
public class OrderService {

  Logger logger = LoggerFactory.getLogger(OrderService.class);

  @PersistenceContext
  EntityManager entityManager;

  public static String[] DEFAULT_COLUMNS = {"facilityCode", "createdDate", "orderNum",
                                            "productName", "productCode", "orderedQuantity",
                                            "filledQuantity"};

  /**
   * Finds orders matching all of provided parameters.
   */
  public List<Order> searchOrders(Facility supplyingFacility, Facility requestingFacility,
                                  Program program, Period period, Schedule schedule,
                                  LocalDate startDate, LocalDate endDate) {
    String hqlQuery = "select o from Order as o, Requisition as r, Period as p "
                      + "where o.supplyingFacility = :supplyingFacility";
    Map<String, Object> params = new HashMap<>();
    params.put("supplyingFacility", supplyingFacility);
    if (requestingFacility != null) {
      hqlQuery += " and o.requestingFacility = :requestingFacility";
      params.put("requestingFacility", requestingFacility);
    }
    if (program != null) {
      hqlQuery += " and o.program = :program";
      params.put("program", program);
    }
    if (period != null) {
      hqlQuery += " and r.processingPeriod = :period";
      params.put("period", period);
    }
    if (schedule != null) {
      hqlQuery += " and p.processingSchedule = :schedule";
      params.put("schedule", schedule);
    }
    if (startDate != null) {
      hqlQuery += " and p.startDate = :startDate";
      params.put("startDate", startDate);
    }
    if (endDate != null) {
      hqlQuery += " and p.endDate = :endDate";
      params.put("endDate", endDate);
    }
    Query query = entityManager.createQuery(hqlQuery);
    Iterator<String> iter = params.keySet().iterator();
    while (iter.hasNext()) {
      String name = iter.next();
      Object value = params.get(name);
      query.setParameter(name, value);
    }
    return query.getResultList();
  }

  /**
   * Changes order to CSV formatted String.
   * @param order Order type object to be transformed into CSV
   * @param chosenColumns String array containing names of columns to be taken from order
   * @return CSV formatted String with an order
   */
  public String orderToCsv(Order order, String[] chosenColumns) {
    if (order != null) {
      OrderService orderService = new OrderService();
      List<Map<String, Object>> rows = orderService.orderToRows(order);
      CsvGenerator generator = new CsvGenerator();

      return generator.toCsv(rows, chosenColumns);
    }

    return null;
  }

  /**
   * Changes order to PDF formatted file given at OutputStream.
   * @param order Order type object to be transformed into CSV
   * @param chosenColumns String array containing names of columns to be taken from order
   * @param out OutputStream to which the pdf file content will be written
   */
  public void orderToPdf(Order order, String[] chosenColumns, OutputStream out) {
    if (order != null) {
      List<Map<String, Object>> rows = orderToRows(order);
      writePdf(order.getOrderCode(), rows, chosenColumns, out);
    }
  }

  private void writePdf(String orderCode, List<Map<String, Object>> rows, String[] chosenColumns,
                        OutputStream out) {
    if (!rows.isEmpty()) {
      Document document = new Document();
      try {
        PdfWriter.getInstance(document, out);
        document.open();
        PdfPTable headerTable = new PdfPTable(1);
        headerTable.setWidthPercentage(100);
        PdfPCell cell = new PdfPCell(new Phrase("Order - " + orderCode));
        cell.setPadding(0);
        cell.setHorizontalAlignment(PdfPCell.ALIGN_CENTER);
        cell.setBorder(PdfPCell.NO_BORDER);
        headerTable.addCell(cell);
        headerTable.setSpacingAfter(10f);
        document.add(headerTable);
        PdfPTable table = new PdfPTable(chosenColumns.length);
        for (String column : chosenColumns) {
          cell = new PdfPCell(new Phrase(column));
          cell.setBackgroundColor(BaseColor.LIGHT_GRAY);
          table.addCell(cell);
        }
        for (Map<String, Object> row : rows) {
          for (String column : chosenColumns) {
            table.addCell(row.get(column).toString());
          }
        }
        document.add(table);
      } catch (DocumentException ex) {
        logger.debug("Error writing pdf file to output stream.", ex);
      } finally {
        document.close();
      }
    }
  }

  private List<Map<String, Object>> orderToRows(Order order) {
    List<Map<String, Object>> rows = new ArrayList<>();
    Set<OrderLine> orderLines = order.getOrderLines();
    String orderNum = order.getOrderCode();
    String facilityCode = order.getRequestingFacility().getCode();
    LocalDateTime createdDate = order.getCreatedDate();

    for (OrderLine orderLine : orderLines) {
      Map<String, Object> row = new HashMap<>();

      row.put(DEFAULT_COLUMNS[0], facilityCode);
      row.put(DEFAULT_COLUMNS[1], createdDate);
      row.put(DEFAULT_COLUMNS[2], orderNum);
      row.put(DEFAULT_COLUMNS[3], orderLine.getProduct().getPrimaryName());
      row.put(DEFAULT_COLUMNS[4], orderLine.getProduct().getCode());
      row.put(DEFAULT_COLUMNS[5], orderLine.getOrderedQuantity());
      row.put(DEFAULT_COLUMNS[6], orderLine.getFilledQuantity());

      //products which have a final approved quantity of zero are omitted
      if (orderLine.getOrderedQuantity() > 0) {
        rows.add(row);
      }
    }
    return rows;
  }
}
