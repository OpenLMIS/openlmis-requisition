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
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

@Service
public class OrderService {

  Logger logger = LoggerFactory.getLogger(OrderService.class);

  @PersistenceContext
  EntityManager entityManager;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private OrderRepository orderRepository;

  public static final String[] DEFAULT_COLUMNS = {"facilityCode", "createdDate", "orderNum",
    "productName", "productCode", "orderedQuantity", "filledQuantity"};

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

  /**
   * Converting Requisition list to Orders.
   */
  @Transactional
  public void convertToOrder(List<Requisition> requisitionList, UUID userId) {
    User user = userRepository.findOne(userId);
    requisitionService.releaseRequisitionsAsOrder(requisitionList);

    for (Requisition requisition : requisitionList) {
      requisition = requisitionRepository.findOne(requisition.getId());

      Order order = new Order();
      order.setCreatedBy(user);
      order.setRequisition(requisition);
      order.setStatus(OrderStatus.ORDERED);

      order.setReceivingFacility(requisition.getFacility());
      order.setRequestingFacility(requisition.getFacility());

      SupplyLine supplyLine = supplyLineRepository.findByProgramAndSupervisoryNode(
          requisition.getProgram(), requisition.getSupervisoryNode());

      order.setSupplyingFacility(supplyLine.getSupplyingFacility());
      order.setProgram(supplyLine.getProgram());

      order.setOrderCode(getOrderCodeFor(requisition, order.getProgram()));
      order.setQuotedCost(BigDecimal.ZERO);

      orderRepository.save(order);

      for (RequisitionLine rl : requisition.getRequisitionLines()) {
        OrderLine orderLine = new OrderLine();
        orderLine.setOrder(order);
        orderLine.setProduct(rl.getProduct());
        orderLine.setFilledQuantity(0L);
        orderLine.setOrderedQuantity(rl.getRequestedQuantity().longValue());
        orderLineRepository.save(orderLine);
      }
    }
  }

  private String getOrderCodeFor(Requisition requisition, Program program) {
    return program.getCode() + requisition.getId() + (requisition.getEmergency() ? "E" : "R");
  }
}
