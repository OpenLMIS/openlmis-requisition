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

import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import com.google.common.collect.Sets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.springframework.beans.factory.annotation.Autowired;

public class StatusChangeRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<StatusChange> {

  @Autowired
  private StatusChangeRepository repository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  private RequisitionTemplate requisitionTemplate;
  private Requisition requisition;
  private UUID userId = UUID.randomUUID();

  StatusChangeRepository getRepository() {
    return this.repository;
  }

  StatusChange generateInstance() {
    return StatusChange.newStatusChange(requisition, userId);
  }

  @Before
  public void setUp() {
    requisitionTemplate = new RequisitionTemplateDataBuilder().build();
    requisitionTemplate = requisitionTemplateRepository.save(requisitionTemplate);

    requisition = generateRequisition();
    requisitionRepository.save(requisition);
  }

  @After
  public void cleanUp() {
    requisitionRepository.delete(requisition);
    requisitionTemplateRepository.delete(requisitionTemplate);
  }

  @Test
  public void shouldFindByRequisitionId() {
    //when
    List<StatusChange> foundStatusChanges = repository.findByRequisitionId(requisition.getId());

    //then
    assertThat(foundStatusChanges.size(), is(1));
    assertEquals(RequisitionStatus.INITIATED, foundStatusChanges.get(0).getStatus());
    assertEquals(userId, foundStatusChanges.get(0).getAuthorId());
  }

  @Test
  public void shouldFindByRequisitionIds() {
    // given
    Requisition requisition1 = generateRequisition();
    requisitionRepository.save(requisition1);

    Set<UUID> ids = Sets.newHashSet(requisition.getId(), requisition1.getId());

    //when
    Map<UUID, List<StatusChange>> groupByRequisitionId = repository
        .findByRequisitionIdIn(ids)
        .stream()
        .collect(Collectors.groupingBy(status -> status.getRequisition().getId()));

    //then
    assertThat(groupByRequisitionId,
        hasEntry(is(requisition.getId()), hasSize(requisition.getStatusChanges().size())));
    assertThat(groupByRequisitionId,
        hasEntry(is(requisition1.getId()), hasSize(requisition1.getStatusChanges().size())));
  }

  private Requisition generateRequisition() {
    Requisition instance = requisition = new RequisitionDataBuilder()
        .withNumberOfMonthsInPeriod(3)
        .withTemplate(requisitionTemplate)
        .buildAsNew();

    instance.setStatusChanges(Collections.singletonList(
        StatusChange.newStatusChange(instance, userId)));

    return instance;
  }

}
