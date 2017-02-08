package org.openlmis.requisition.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.time.ZonedDateTime;
import java.util.UUID;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;
import org.junit.Ignore;
import org.junit.Test;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.JasperTemplateRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;

public class ReportsControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String REQUISITION_TEMPLATE_PATH = "jasperTemplates/requisition.jrxml";
  private static final String REQUISITION_TEMPLATE_NAME = "Print Requisition";
  private static final String PRINT_URL = "/api/requisitions/{id}/print";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private JasperTemplateRepository jasperTemplateRepository;

  @Test
  public void shouldNotPrintRequisitionIfDoesNotExist() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(404);
  }

  @Test
  public void shouldNotPrintRequisitionIfTemplateDoesNotExist() {
    Requisition requisition = generateRequisition();
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(400);
  }

  @Ignore // TODO: OLMIS-1182 Re-enable once Javers adds dates to Requisition/RequisitionDto
  @Test
  public void shouldPrintRequisition() throws IOException, JRException {
    Requisition requisition = generateRequisition();
    generateJasperTemplate();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(200);
  }

  private Requisition generateRequisition() {
    RequisitionTemplate template = requisitionTemplateRepository.save(new RequisitionTemplate());
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), RequisitionStatus.INITIATED, true);

    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);
    return requisitionRepository.save(requisition);
  }

  private JasperTemplate generateJasperTemplate() throws IOException, JRException {
    JasperTemplate template = new JasperTemplate();
    template.setName(REQUISITION_TEMPLATE_NAME);
    template.setData(getTemplateData(REQUISITION_TEMPLATE_PATH));
    return jasperTemplateRepository.save(template);
  }


  private byte[] getTemplateData(String path) throws IOException, JRException {
    ClassPathResource resource = new ClassPathResource(path);
    JasperReport report = JasperCompileManager.compileReport(resource.getInputStream());

    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    ObjectOutputStream out = new ObjectOutputStream(bos);
    out.writeObject(report);

    return bos.toByteArray();
  }
}
