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
import static org.hamcrest.Matchers.isA;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.util.List;
import java.util.UUID;
import javax.persistence.EntityManager;
import org.apache.commons.lang.RandomStringUtils;
import org.hibernate.exception.ConstraintViolationException;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StatusMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.annotation.Commit;

public class StatusMessageRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<StatusMessage> {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Autowired
  StatusMessageRepository repository;

  @Autowired
  StatusChangeRepository statusChangeRepository;
  
  @Autowired
  RequisitionRepository requisitionRepository;
  
  @Autowired
  RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  EntityManager entityManager;
  
  private RequisitionTemplate requisitionTemplate;
  private Requisition requisition;
  private UUID userId = UUID.randomUUID();
  private String userFirstName = "FirstName";
  private String userLastName = "LastName";

  StatusMessageRepository getRepository() {
    return this.repository;
  }

  StatusMessage generateInstance() {
    StatusChange statusChange = StatusChange.newStatusChange(requisition, userId);
    statusChangeRepository.save(statusChange);

    return StatusMessage.newStatusMessage(requisition, statusChange,
        userId, userFirstName, userLastName, RandomStringUtils.randomAlphanumeric(500));
  }
  
  @Before
  public void setUp() {
    requisitionTemplate = new RequisitionTemplateDataBuilder().build();
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

  @Test
  @Commit
  public void shouldFailToSaveWithDuplicateStatusChange() {
    expectedException.expectCause(isA(ConstraintViolationException.class));

    // given
    StatusMessage statusMessage = this.generateInstance();
    repository.save(statusMessage);

    StatusMessage duplicateStatusMessage = this.generateInstance();
    duplicateStatusMessage.setStatusChange(statusMessage.getStatusChange());

    // when
    repository.save(duplicateStatusMessage);
    entityManager.flush();
  }
}
