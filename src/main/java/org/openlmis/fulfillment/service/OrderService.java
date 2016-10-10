package org.openlmis.fulfillment.service;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.data.JRMapArrayDataSource;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLineItem;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.exception.OrderCsvWriteException;
import org.openlmis.fulfillment.exception.OrderPdfWriteException;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.io.ICsvMapWriter;
import org.supercsv.prefs.CsvPreference;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static ch.qos.logback.core.util.CloseUtil.closeQuietly;

@Service
public class OrderService {

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  public static final String[] DEFAULT_COLUMNS = {"facilityCode", "createdDate", "orderNum",
    "productName", "productCode", "orderedQuantity", "filledQuantity"};

  /**
   * Finds orders matching all of provided parameters.
   * @param supplyingFacility supplyingFacility of searched Orders.
   * @param requestingFacility requestingFacility of searched Orders.
   * @param program program of searched Orders.
   * @return ist of Orders with matched parameters.
   */
  public List<Order> searchOrders(UUID supplyingFacility, UUID requestingFacility,
                                  UUID program) {
    return orderRepository.searchOrders(
            supplyingFacility,
            requestingFacility,
            program);
  }

  /**
   * Changes order to CSV formatted file.
   * @param order Order type object to be transformed into CSV
   * @param chosenColumns String array containing names of columns to be taken from order
   */
  public void orderToCsv(Order order, String[] chosenColumns,
                         Writer writer) throws OrderCsvWriteException {
    if (order != null) {
      List<Map<String, Object>> rows = orderToRows(order);

      if (!rows.isEmpty()) {
        ICsvMapWriter mapWriter = null;
        try {
          mapWriter = new CsvMapWriter(writer, CsvPreference.STANDARD_PREFERENCE);
          mapWriter.writeHeader(chosenColumns);

          for (Map<String, Object> row : rows) {
            mapWriter.write(row, chosenColumns);
          }
        } catch (IOException ex) {
          throw new OrderCsvWriteException("I/O while creating the order CSV file", ex);
        } finally {
          closeQuietly(mapWriter);
        }
      }
    }
  }

  /**
   * Changes order to PDF formatted file given at OutputStream.
   * @param order Order type object to be transformed into CSV
   * @param chosenColumns String array containing names of columns to be taken from order
   * @param out OutputStream to which the pdf file content will be written
   */
  public void orderToPdf(Order order, String[] chosenColumns, OutputStream out)
          throws OrderPdfWriteException {
    if (order != null) {
      List<Map<String, Object>> rows = orderToRows(order);
      try {
        writePdf(rows, chosenColumns, out);
      } catch (JRException ex) {
        throw new OrderPdfWriteException("Jasper error", ex);
      } catch (IOException ex) {
        throw new OrderPdfWriteException("I/O error", ex);
      }
    }
  }

  //TODO: fix this temporary method after JasperTemplate class is finished
  private void writePdf(List<Map<String, Object>> data, String[] chosenColumns,
                        OutputStream out) throws JRException, IOException {
    ClassLoader classLoader = getClass().getClassLoader();
    File template = new File(
            classLoader.getResource(
                    "jasperTemplates/ordersJasperTemplate.jrxml").getFile());

    try (FileInputStream fis = new FileInputStream(template)) {
      JasperReport pdfTemplate = JasperCompileManager.compileReport(fis);
      HashMap<String, Object>[] params = new HashMap[data.size()];
      int index = 0;
      for (Map<String, Object> dataRow : data) {
        params[index] = new HashMap<>();
        params[index].put(DEFAULT_COLUMNS[3], dataRow.get(DEFAULT_COLUMNS[3]));
        params[index].put(DEFAULT_COLUMNS[6], dataRow.get(DEFAULT_COLUMNS[6]));
        params[index].put(DEFAULT_COLUMNS[5], dataRow.get(DEFAULT_COLUMNS[5]));
        index++;
      }
      JRMapArrayDataSource dataSource = new JRMapArrayDataSource(params);
      JasperPrint jasperPrint = JasperFillManager.fillReport(pdfTemplate, new HashMap<>(),
              dataSource);
      JRPdfExporter exporter = new JRPdfExporter();
      exporter.setExporterInput(new SimpleExporterInput(jasperPrint));
      exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
      exporter.exportReport();
    }
  }

  private List<Map<String, Object>> orderToRows(Order order) {
    List<Map<String, Object>> rows = new ArrayList<>();

    List<OrderLineItem> orderLineItems = order.getOrderLineItems();
    String orderNum = order.getOrderCode();

    FacilityDto requestingFacility = facilityReferenceDataService.findOne(
            order.getRequestingFacilityId());
    String facilityCode = requestingFacility.getCode();
    LocalDateTime createdDate = order.getCreatedDate();

    for (OrderLineItem orderLineItem : orderLineItems) {
      Map<String, Object> row = new HashMap<>();

      OrderableProductDto product = orderableProductReferenceDataService
          .findOne(orderLineItem.getOrderableProductId());

      row.put(DEFAULT_COLUMNS[0], facilityCode);
      row.put(DEFAULT_COLUMNS[1], createdDate);
      row.put(DEFAULT_COLUMNS[2], orderNum);
      row.put(DEFAULT_COLUMNS[3], product.getName());
      row.put(DEFAULT_COLUMNS[4], product.getProductCode());
      row.put(DEFAULT_COLUMNS[5], orderLineItem.getOrderedQuantity());
      row.put(DEFAULT_COLUMNS[6], orderLineItem.getFilledQuantity());

      //products which have a final approved quantity of zero are omitted
      if (orderLineItem.getOrderedQuantity() > 0) {
        rows.add(row);
      }
    }
    return rows;
  }

  /**
   * Converting Requisition list to Orders.
   */
  @Transactional
  public List<Order> convertToOrder(List<Requisition> requisitionList, UUID userId)
          throws RequisitionException {
    UserDto user = userReferenceDataService.findOne(userId);
    List<Requisition> releasedRequisitions =
        requisitionService.releaseRequisitionsAsOrder(requisitionList, user);

    return releasedRequisitions.stream().map(r -> createFromRequisition(r, user))
        .collect(Collectors.toList());
  }

  /**
   * Creates an order based on given requisition.
   * @param requisition requisition to initialize order
   * @return created order
   */
  private Order createFromRequisition(Requisition requisition, UserDto user) {
    Order order = new Order(requisition);
    order.setCreatedById(user.getId());

    ProgramDto program = programReferenceDataService.findOne(order.getProgramId());
    OrderNumberConfiguration orderNumberConfiguration =
        orderNumberConfigurationRepository.findAll().iterator().next();

    order.setOrderCode(orderNumberConfiguration.generateOrderNumber(requisition, program));

    orderRepository.save(order);
    return order;
  }
}
