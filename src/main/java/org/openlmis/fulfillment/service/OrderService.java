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
import org.openlmis.csv.generator.CsvGenerator;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
public class OrderService {

  Logger logger = LoggerFactory.getLogger(OrderService.class);

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

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
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  public static final String[] DEFAULT_COLUMNS = {"facilityCode", "createdDate", "orderNum",
    "productName", "productCode", "orderedQuantity", "filledQuantity"};

  /**
   * Finds orders matching all of provided parameters.
   * @param supplyingFacility supplyingFacility of searched Orders.
   * @param requestingFacility requestingFacility of searched Orders.
   * @param program program of searched Orders.
   * @return ist of Orders with matched parameters.
   */
  public List<Order> searchOrders(FacilityDto supplyingFacility, FacilityDto requestingFacility,
                                  ProgramDto program) {
    return orderRepository.searchOrders(
            supplyingFacility,
            requestingFacility,
            program);
  }

  /**
   * Changes order to CSV formatted String.
   * @param order Order type object to be transformed into CSV
   * @param chosenColumns String array containing names of columns to be taken from order
   * @return CSV formatted String with an order
   */
  public String orderToCsv(Order order, String[] chosenColumns) {
    if (order != null) {
      List<Map<String, Object>> rows = orderToRows(order);
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
      writePdf(rows, chosenColumns, out);
    }
  }

  //TODO: fix this temporary method after JasperTemplate class is finished
  private void writePdf(List<Map<String, Object>> data, String[] chosenColumns,
                        OutputStream out) {
    try {
      ClassLoader classLoader = getClass().getClassLoader();
      File template = new File(
          classLoader.getResource(
              "jasperTemplates/ordersJasperTemplate.jrxml").getFile());

      FileInputStream fis = new FileInputStream(template);
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
    } catch (JRException ex) {
      logger.debug("Error compiling jasper template.", ex);
    } catch (FileNotFoundException ex) {
      logger.debug("Error reading from file.", ex);
    } catch (NullPointerException ex) {
      logger.debug("File does not exist." , ex);
    }
  }

  private List<Map<String, Object>> orderToRows(Order order) {
    List<Map<String, Object>> rows = new ArrayList<>();
    List<OrderLine> orderLines = order.getOrderLines();
    String orderNum = order.getOrderCode();
    FacilityDto requestingFacility = facilityReferenceDataService.findOne(
            order.getRequestingFacility());
    String facilityCode = requestingFacility.getCode();
    LocalDateTime createdDate = order.getCreatedDate();

    for (OrderLine orderLine : orderLines) {
      Map<String, Object> row = new HashMap<>();

      row.put(DEFAULT_COLUMNS[0], facilityCode);
      row.put(DEFAULT_COLUMNS[1], createdDate);
      row.put(DEFAULT_COLUMNS[2], orderNum);
      /**
      row.put(DEFAULT_COLUMNS[3], orderLine.getProduct().getPrimaryName());
      row.put(DEFAULT_COLUMNS[4], orderLine.getProduct().getCode());
       **/
      row.put(DEFAULT_COLUMNS[5], orderLine.getOrderedQuantity());
      row.put(DEFAULT_COLUMNS[6], orderLine.getFilledQuantity());

      //products which have a final approved quantity of zero are omitted
      if (orderLine.getOrderedQuantity() > 0) {
        rows.add(row);
      }
    }
    return rows;
  }

  /**
   * Converting Requisition list to Orders.
   */
  @Transactional
  public List<Order> convertToOrder(List<Requisition> requisitionList, UUID userId) {
    UserDto user = userReferenceDataService.findOne(userId);
    requisitionService.releaseRequisitionsAsOrder(requisitionList);
    List<Order> convertedOrders = new ArrayList<>();

    for (Requisition requisition : requisitionList) {
      requisition = requisitionRepository.findOne(requisition.getId());

      Order order = new Order();
      order.setCreatedById(user.getId());
      order.setRequisition(requisition);
      order.setStatus(OrderStatus.ORDERED);

      order.setReceivingFacility(requisition.getFacility());
      order.setRequestingFacility(requisition.getFacility());

      List<SupplyLineDto> supplyLines = supplyLineReferenceDataService
          .search(requisition.getProgram(),
              requisition.getSupervisoryNode());
      SupplyLineDto supplyLine = supplyLines.get(0);

      order.setSupplyingFacility(supplyLine.getSupplyingFacility().getId());
      order.setProgram(supplyLine.getProgram().getId());

      OrderNumberConfiguration orderNumberConfiguration =
          orderNumberConfigurationRepository.findAll().iterator().next();

      ProgramDto program = programReferenceDataService.findOne(order.getProgram());

      order.setOrderCode(orderNumberConfiguration.generateOrderNumber(
          requisition.getId(), program.getCode(), requisition.getEmergency()));

      order.setQuotedCost(BigDecimal.ZERO);

      orderRepository.save(order);

      List<OrderLine> orderLines = new ArrayList<>();
      for (RequisitionLine rl : requisition.getRequisitionLines()) {
        OrderLine orderLine = new OrderLine();
        orderLine.setOrder(order);
        orderLine.setProduct(rl.getProduct());
        orderLine.setFilledQuantity(0L);
        orderLine.setOrderedQuantity(rl.getRequestedQuantity().longValue());
        orderLines.add(orderLine);
        orderLineRepository.save(orderLine);
      }
      order.setOrderLines(orderLines);
      convertedOrders.add(order);
    }
    return convertedOrders;
  }
}
