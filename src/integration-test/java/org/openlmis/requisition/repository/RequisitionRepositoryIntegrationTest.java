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

import static java.util.Collections.singleton;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.Sets;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.CurrencyConfig;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  @Autowired
  private RequisitionRepository repository;

  @Autowired
  private RequisitionTemplateRepository templateRepository;
  
  private RequisitionTemplate testTemplate;

  private List<Requisition> requisitions;

  @Override
  RequisitionRepository getRepository() {
    return this.repository;
  }

  @Override
  Requisition generateInstance() {
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), RequisitionStatus.INITIATED, getNextInstanceNumber() % 2 == 0);
    requisition.setCreatedDate(ZonedDateTime.now().plusDays(requisitions.size()));
    requisition.setSupervisoryNodeId(UUID.randomUUID());
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setTemplate(testTemplate);
    List<StatusChange> statusChanges = new ArrayList<>();
    statusChanges.add(StatusChange.newStatusChange(requisition, UUID.randomUUID()));
    requisition.setStatusChanges(statusChanges);
    return requisition;
  }

  @Before
  public void setUp() {
    testTemplate = templateRepository.save(new RequisitionTemplate());
    requisitions = new ArrayList<>();
    for (int count = 0; count < 5; ++count) {
      requisitions.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchRequisitionsByAllProperties() {
    Requisition requisition = new Requisition(requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(), requisitions.get(0).getProcessingPeriodId(),
        requisitions.get(0).getStatus(), requisitions.get(0).getEmergency());
    requisition.setSupervisoryNodeId(requisitions.get(0).getSupervisoryNodeId());
    requisition.setTemplate(testTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setStatusChanges(Collections.singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(),
        requisitions.get(0).getCreatedDate().minusDays(1),
        requisitions.get(0).getCreatedDate().plusDays(2),
        requisitions.get(0).getProcessingPeriodId(),
        requisitions.get(0).getSupervisoryNodeId(),
        EnumSet.of(requisitions.get(0).getStatus()),
        requisitions.get(0).getEmergency());

    assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      assertEquals(
          receivedRequisition.getProgramId(),
          requisitions.get(0).getProgramId());
      assertEquals(
          receivedRequisition.getProcessingPeriodId(),
          requisitions.get(0).getProcessingPeriodId());
      assertEquals(
          receivedRequisition.getFacilityId(),
          requisitions.get(0).getFacilityId());
      assertEquals(
          receivedRequisition.getSupervisoryNodeId(),
          requisitions.get(0).getSupervisoryNodeId());
      assertEquals(
          receivedRequisition.getStatus(),
          requisitions.get(0).getStatus());
      assertTrue(
          receivedRequisition.getCreatedDate().isBefore(
              requisitions.get(0).getCreatedDate().plusDays(2)));
      assertTrue(
          receivedRequisition.getCreatedDate().isAfter(
              requisitions.get(0).getCreatedDate().minusDays(1)));
      assertEquals(receivedRequisition.getEmergency(), requisitions.get(0).getEmergency());
      assertEquals(receivedRequisition.getNumberOfMonthsInPeriod(), Integer.valueOf(1));
    }
  }

  @Test
  public void testSearchRequisitionsByFacilityAndProgram() {
    Requisition requisition = new Requisition(requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(), requisitions.get(0).getProcessingPeriodId(),
        requisitions.get(0).getStatus(), false);
    requisition.setSupervisoryNodeId(requisitions.get(0).getSupervisoryNodeId());
    requisition.setTemplate(testTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setStatusChanges(Collections.singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(),
        null, null, null, null, null, null);

    assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      assertEquals(
          receivedRequisition.getProgramId(),
          requisitions.get(0).getProgramId());
      assertEquals(
          receivedRequisition.getFacilityId(),
          requisitions.get(0).getFacilityId());
      assertEquals(
          receivedRequisition.getNumberOfMonthsInPeriod(), Integer.valueOf(1));
    }
  }

  @Test
  public void testSearchRequisitionsByAllParametersNull() {
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        null, null, null, null, null, null, null, null);

    assertEquals(5, receivedRequisitions.size());
  }

  @Test
  public void testSearchEmergencyRequsitions() {
    List<Requisition> emergency = repository.searchRequisitions(
        null, null, null, null, null, null, null, true);

    assertEquals(2, emergency.size());
    emergency.forEach(requisition -> assertTrue(requisition.getEmergency()));
  }

  @Test
  public void testSearchStandardRequisitions() {
    List<Requisition> standard = repository.searchRequisitions(
        null, null, null, null, null, null, null, false);

    assertEquals(3, standard.size());
    standard.forEach(requisition -> assertFalse(requisition.getEmergency()));
  }

  @Test
  public void testSearchRequisitionsByPeriodAndEmergencyFlag() {
    requisitions.forEach(requisition -> {
      List<Requisition> found = repository.searchRequisitions(
          requisition.getProcessingPeriodId(), requisition.getFacilityId(), requisition
              .getProgramId(), requisition.getEmergency()
      );

      found.forEach(element -> {
        assertEquals(requisition.getProcessingPeriodId(), element.getProcessingPeriodId());
        assertEquals(requisition.getEmergency(), element.getEmergency());
        assertEquals(requisition.getNumberOfMonthsInPeriod(), element.getNumberOfMonthsInPeriod());
      });
    });
  }

  @Test
  public void testFindRequisitionsCountByPeriodFacilityProgramAndEmergencyFlag() {
    requisitions.forEach(requisition -> {
      int count = getRequisitionsCount(requisition);
      assertEquals(1, count);
    });

    Requisition entity = generateInstance();
    repository.save(entity);
    Requisition entity2 = generateInstanceBasedOn(entity);
    repository.save(entity2);

    int count = getRequisitionsCount(entity);
    assertEquals(2, count);
  }

  @Test
  public void shouldNotFindAnyRequisition() {
    assertEquals(0, repository.getRequisitionsCount(
        UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), true));
  }

  @Test
  public void testSearchRequisitionsByTemplate() {
    // given
    RequisitionTemplate nonMatchingTemplate = templateRepository.save(new RequisitionTemplate());
    Requisition nonMatchingRequisition = requisitions.get(0);
    nonMatchingRequisition.setTemplate(nonMatchingTemplate);

    List<Requisition> matchingRequisitions =
        requisitions.stream().skip(1).collect(Collectors.toList());
    matchingRequisitions.forEach(r -> r.setTemplate(testTemplate));

    requisitions.forEach(r -> repository.save(r));

    // when
    List<Requisition> result = repository.findByTemplateId(testTemplate.getId());

    // then
    assertEquals(matchingRequisitions.size(), result.size());
  }

  @Test
  public void shouldPersistWithMoney() {
    Money pricePerPack = Money.of(CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE), 14.57);

    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());

    ProgramOrderableDto programOrderable = new ProgramOrderableDto();
    programOrderable.setPricePerPack(pricePerPack);
    programOrderable.setProgramId(program.getId());

    OrderableDto orderable = new OrderableDto();
    orderable.setId(UUID.randomUUID());
    orderable.setPrograms(Sets.newHashSet(programOrderable));

    ApprovedProductDto ftap = new ApprovedProductDto();
    ftap.setOrderable(orderable);
    ftap.setProgram(program);
    ftap.setMaxPeriodsOfStock(7.25);

    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), RequisitionStatus.INITIATED, false);
    requisition.initiate(setUpTemplateWithBeginningBalance(), singleton(ftap),
        Collections.emptyList(), 0, null, UUID.randomUUID());

    requisition = repository.save(requisition);
    requisition = repository.findOne(requisition.getId());

    assertEquals(pricePerPack, requisition.getRequisitionLineItems().get(0).getPricePerPack());
  }

  @Test
  public void shouldPersistWithPreviousRequisitions() {
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), RequisitionStatus.INITIATED, false);
    requisition.setPreviousRequisitions(requisitions);

    requisition = repository.save(requisition);
    requisition = repository.findOne(requisition.getId());

    assertEquals(5, requisition.getPreviousRequisitions().size());
  }

  @Test
  public void shouldRetrieveLastRegularRequisition() {
    final UUID clinic = UUID.randomUUID();
    final UUID hospital = UUID.randomUUID();
    final UUID essentialMeds = UUID.randomUUID();
    final UUID familyPlanning = UUID.randomUUID();

    repository.save(generateRequisition(clinic, essentialMeds));
    repository.save(generateRequisition(clinic, essentialMeds));
    Requisition thirdClinicEm = generateRequisition(clinic, essentialMeds);
    thirdClinicEm = repository.save(thirdClinicEm);

    repository.save(generateRequisition(clinic, familyPlanning));
    Requisition secondClinicFp = generateRequisition(clinic, familyPlanning);
    secondClinicFp = repository.save(secondClinicFp);

    Requisition firstHospitalEm = generateRequisition(hospital, essentialMeds);
    firstHospitalEm = repository.save(firstHospitalEm);

    repository.save(generateRequisition(hospital, familyPlanning));
    repository.save(generateRequisition(hospital, familyPlanning));
    Requisition thirdHospitalFp = generateRequisition(hospital, familyPlanning);
    thirdHospitalFp = repository.save(thirdHospitalFp);

    Requisition lastRequisition = repository.getLastRegularRequisition(clinic, essentialMeds);
    assertNotNull(lastRequisition);
    assertEquals(thirdClinicEm.getId(), lastRequisition.getId());

    lastRequisition = repository.getLastRegularRequisition(clinic, familyPlanning);
    assertNotNull(lastRequisition);
    assertEquals(secondClinicFp.getId(), lastRequisition.getId());

    lastRequisition = repository.getLastRegularRequisition(hospital, essentialMeds);
    assertNotNull(lastRequisition);
    assertEquals(firstHospitalEm.getId(), lastRequisition.getId());

    lastRequisition = repository.getLastRegularRequisition(hospital, familyPlanning);
    assertNotNull(lastRequisition);
    assertEquals(thirdHospitalFp.getId(), lastRequisition.getId());
  }

  private Requisition generateRequisition(UUID facility, UUID program) {
    Requisition requisition = new Requisition(facility, program, UUID.randomUUID(),
        RequisitionStatus.INITIATED, false);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setTemplate(testTemplate);

    return requisition;
  }

  private RequisitionTemplate setUpTemplateWithBeginningBalance() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();
    column.setName(RequisitionLineItem.BEGINNING_BALANCE);
    column.setIsDisplayed(true);

    return templateRepository.save(new RequisitionTemplate(
        Collections.singletonMap(RequisitionLineItem.BEGINNING_BALANCE, column)));
  }

  private Requisition generateInstanceBasedOn(Requisition entity) {
    Requisition entity2 = generateInstance();
    entity2.setProcessingPeriodId(entity.getProcessingPeriodId());
    entity2.setFacilityId(entity.getFacilityId());
    entity2.setProgramId(entity.getProgramId());
    entity2.setEmergency(entity.getEmergency());
    return entity2;
  }

  private int getRequisitionsCount(Requisition entity) {
    return repository.getRequisitionsCount(
        entity.getProcessingPeriodId(), entity.getFacilityId(), entity
            .getProgramId(), entity.getEmergency()
    );
  }
}
