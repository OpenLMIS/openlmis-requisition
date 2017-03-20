/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.web;

import net.sf.jasperreports.engine.JRException;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.time.ZonedDateTime;
import java.util.UUID;

public class ReportsControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String PRINT_URL = "/api/requisitions/{id}/print";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

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
  public void shouldPrintRequisition() throws IOException, JRException {
    Requisition requisition = generateRequisition();

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
}
