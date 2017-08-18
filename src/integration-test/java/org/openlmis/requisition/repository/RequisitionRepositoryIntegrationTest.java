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

import static com.google.common.collect.Lists.newArrayList;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;

import com.google.common.collect.Sets;
import org.apache.commons.lang.RandomStringUtils;
import org.assertj.core.util.Lists;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.CurrencyConfig;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonType;
import org.springframework.beans.factory.annotation.Autowired;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceException;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  @Autowired
  private RequisitionRepository repository;

  @Autowired
  private RequisitionTemplateRepository templateRepository;

  @Autowired
  private EntityManager entityManager;
  
  private RequisitionTemplate testTemplate;

  private List<Requisition> requisitions;

  @Override
  RequisitionRepository getRepository() {
    return this.repository;
  }

  @Override
  Requisition generateInstance() {
    return generateInstance(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID());
  }

  private Requisition generateInstance(UUID facilityId, UUID programId, UUID processingPeriodId) {
    Requisition requisition = new Requisition(facilityId, programId, processingPeriodId,
            INITIATED, getNextInstanceNumber() % 2 == 0);
    requisition.setCreatedDate(ZonedDateTime.now().plusDays(requisitions.size()));
    requisition.setSupervisoryNodeId(UUID.randomUUID());
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setTemplate(testTemplate);
    List<StatusChange> statusChanges = new ArrayList<>();
    statusChanges.add(StatusChange.newStatusChange(requisition, UUID.randomUUID()));
    requisition.setStatusChanges(statusChanges);

    StockAdjustmentReason reason = generateStockAdjustmentReason();
    requisition.setStockAdjustmentReasons(newArrayList(reason));

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
    // for this test we need an emergency requisition
    // so that the uniqueness constraint is not violated
    Requisition requisitionToCopy = requisitions.get(1);

    Requisition requisition = new Requisition(requisitionToCopy.getFacilityId(),
        requisitionToCopy.getProgramId(), requisitionToCopy.getProcessingPeriodId(),
        requisitionToCopy.getStatus(), requisitionToCopy.getEmergency());
    requisition.setSupervisoryNodeId(requisitionToCopy.getSupervisoryNodeId());
    requisition.setTemplate(testTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setStatusChanges(Collections.singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));
    requisition.setEmergency(true);
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitionToCopy.getFacilityId(),
        requisitionToCopy.getProgramId(),
        requisitionToCopy.getCreatedDate().minusDays(1),
        requisitionToCopy.getCreatedDate().plusDays(2),
        requisitionToCopy.getProcessingPeriodId(),
        requisitionToCopy.getSupervisoryNodeId(),
        EnumSet.of(requisitionToCopy.getStatus()),
        requisitionToCopy.getEmergency());

    assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      assertEquals(
          receivedRequisition.getProgramId(),
          requisitionToCopy.getProgramId());
      assertEquals(
          receivedRequisition.getProcessingPeriodId(),
          requisitionToCopy.getProcessingPeriodId());
      assertEquals(
          receivedRequisition.getFacilityId(),
          requisitionToCopy.getFacilityId());
      assertEquals(
          receivedRequisition.getSupervisoryNodeId(),
          requisitionToCopy.getSupervisoryNodeId());
      assertEquals(
          receivedRequisition.getStatus(),
          requisitionToCopy.getStatus());
      assertTrue(
          receivedRequisition.getCreatedDate().isBefore(
              requisitionToCopy.getCreatedDate().plusDays(2)));
      assertTrue(
          receivedRequisition.getCreatedDate().isAfter(
              requisitionToCopy.getCreatedDate().minusDays(1)));
      assertEquals(receivedRequisition.getNumberOfMonthsInPeriod(), Integer.valueOf(1));
      assertEquals(receivedRequisition.getEmergency(), requisitionToCopy.getEmergency());
    }
  }

  @Test
  public void testSearchRequisitionsByFacilityAndProgram() {
    Requisition requisition = new Requisition(requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(), UUID.randomUUID(),
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
  public void shouldFindRequisitionsByMultipleIds() {
    UUID id1 = requisitions.get(0).getId();
    UUID id2 = requisitions.get(1).getId();

    List<Requisition> foundRequisitions = Lists.newArrayList(repository.findAll(
        Lists.newArrayList(id1, id2, UUID.randomUUID())));
    assertNotNull(foundRequisitions);
    assertEquals(2, foundRequisitions.size());
    assertEquals(requisitions.get(0), foundRequisitions.get(0));
    assertEquals(requisitions.get(1), foundRequisitions.get(1));
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
        UUID.randomUUID(), INITIATED, false);
    requisition.initiate(setUpTemplateWithBeginningBalance(), singleton(ftap),
        Collections.emptyList(), 0, null, UUID.randomUUID());

    requisition = repository.save(requisition);
    requisition = repository.findOne(requisition.getId());

    assertEquals(pricePerPack, requisition.getRequisitionLineItems().get(0).getPricePerPack());
  }

  @Test
  public void shouldPersistWithPreviousRequisitions() {
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), INITIATED, false);
    requisition.setPreviousRequisitions(requisitions);

    requisition = repository.save(requisition);
    requisition = repository.findOne(requisition.getId());

    assertEquals(5, requisition.getPreviousRequisitions().size());
  }

  @Test(expected = PersistenceException.class)
  public void shouldNotAllowMultipleRegularRequisitionForFacilityProgramPeriod() {
    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID periodId = UUID.randomUUID();

    Requisition requisition1 =  generateInstance(facilityId, programId, periodId);
    Requisition requisition2 = generateInstance(facilityId, programId, periodId);

    requisition1.setEmergency(false);
    requisition2.setEmergency(false);

    repository.save(asList(requisition1, requisition2));

    entityManager.flush();
  }

  @Test
  public void shouldAllowMultipleEmergencyRequisitionsForFacilityProgramPeriod() {
    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID periodId = UUID.randomUUID();

    Requisition regularRequisition =  generateInstance(facilityId, programId, periodId);
    Requisition emergencyRequisition1 = generateInstance(facilityId, programId, periodId);
    Requisition emergencyRequisition2 = generateInstance(facilityId, programId, periodId);

    emergencyRequisition1.setEmergency(true);
    emergencyRequisition2.setEmergency(true);
    regularRequisition.setEmergency(false);

    repository.save(asList(regularRequisition, emergencyRequisition1, emergencyRequisition2));

    entityManager.flush();
  }

  private StockAdjustmentReason generateStockAdjustmentReason() {
    StockAdjustmentReason reason = new StockAdjustmentReason();
    reason.setReasonCategory(ReasonCategory.ADJUSTMENT);
    reason.setReasonType(ReasonType.BALANCE_ADJUSTMENT);
    reason.setDescription("simple description");
    reason.setIsFreeTextAllowed(false);
    reason.setName(RandomStringUtils.random(5));
    return reason;
  }

  private RequisitionTemplate setUpTemplateWithBeginningBalance() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();
    column.setName(RequisitionLineItem.BEGINNING_BALANCE);
    column.setIsDisplayed(true);

    return templateRepository.save(new RequisitionTemplate(
        Collections.singletonMap(RequisitionLineItem.BEGINNING_BALANCE, column)));
  }
}
