package org.openlmis.download;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CsvOrderGenerator {

  public static String[] DEFAULT_COLUMNS = {"facilityCode", "createdDate",
      "orderNum", "productName", "productCode", "orderedQuantity"};

  /**
   * Method prepares csv-formatted rows from Order object and
   *        then hands over this rows to CsvGenerator.
   *
   * @param order to be saved in CSV format
   * @param chosenColumns - array of chosen columns names which
   *        defines what fields of Object will be written to CSV
   */
  public String orderToCsv(Order order, String[] chosenColumns) {
    List<Map<String, Object>> csvRows = orderToCsvRows(order);

    CsvGenerator generator = new CsvGenerator();

    return generator.toCsv(csvRows, chosenColumns);
  }

  private List<Map<String, Object>> orderToCsvRows(Order order) {
    List<Map<String, Object>> csvRows = new ArrayList<>();
    Set<OrderLine> orderLines = order.getOrderLines();
    String orderNum = order.getOrderCode();
    String facilityCode = order.getRequestingFacility().getCode();
    LocalDateTime createdDate = order.getCreatedDate();

    for (OrderLine orderLine : orderLines) {
      Map<String, Object> row = new HashMap<>();

      row.put(DEFAULT_COLUMNS[0], facilityCode);
      row.put(DEFAULT_COLUMNS[1], createdDate);
      row.put(DEFAULT_COLUMNS[2],orderNum);
      row.put(DEFAULT_COLUMNS[3], orderLine.getProduct().getPrimaryName());
      row.put(DEFAULT_COLUMNS[4], orderLine.getProduct().getCode());
      row.put(DEFAULT_COLUMNS[5], orderLine.getOrderedQuantity());

      //products which have a final approved quantity of zero are omitted
      if (orderLine.getOrderedQuantity() > 0) {
        csvRows.add(row);
      }
    }

    return csvRows;
  }
}
