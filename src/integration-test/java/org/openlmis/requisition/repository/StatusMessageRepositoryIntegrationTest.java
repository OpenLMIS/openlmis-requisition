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

package org.openlmis.requisition.repository;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StatusMessage;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.UUID;

public class StatusMessageRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<StatusMessage> {

  @Autowired
  StatusMessageRepository repository;
  
  @Autowired
  RequisitionRepository requisitionRepository;
  
  @Autowired
  RequisitionTemplateRepository requisitionTemplateRepository;
  
  private RequisitionTemplate requisitionTemplate;
  private Requisition requisition;
  private UUID userId = UUID.randomUUID();
  private String userFirstName = "FirstName";
  private String userLastName = "LastName";

  StatusMessageRepository getRepository() {
    return this.repository;
  }

  StatusMessage generateInstance() {
    return StatusMessage.newStatusMessage(requisition, userId, userFirstName, userLastName,
        "Status Message");
  }
  
  @Before
  public void setUp() {
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate = requisitionTemplateRepository.save(requisitionTemplate);

    requisition = new Requisition(
        UUID.randomUUID(),
        UUID.randomUUID(),
        UUID.randomUUID(),
        RequisitionStatus.INITIATED,
        false);
    requisition.setNumberOfMonthsInPeriod(3);
    requisition.setTemplate(requisitionTemplate);
    
    requisitionRepository.save(requisition);
  }
  
  @After
  public void cleanUp() {
    requisitionRepository.delete(requisition);
    requisitionTemplateRepository.delete(requisitionTemplate);
  }
  
  @Test
  public void shouldFindByRequisitionId() {
    //given
    StatusMessage statusMessage = this.generateInstance();
    repository.save(statusMessage);

    //when
    List<StatusMessage> foundStatusMessages = repository.findByRequisitionId(requisition.getId());

    //then
    assertThat(foundStatusMessages.size(), is(1));
    assertEquals(statusMessage, foundStatusMessages.get(0));
  }
}