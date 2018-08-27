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

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.PersistenceException;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.assertj.core.util.Lists;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.CurrencyConfig;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StockAdjustment;
import org.openlmis.requisition.domain.requisition.StockData;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseRequisitionRepositoryIntegrationTest {

  private List<Requisition> requisitions;
  
  private Pageable pageRequest = new PageRequest(
      Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION);

  @Before
  public void setUp() {
    testTemplate = templateRepository.save(new RequisitionTemplateDataBuilder().build());
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
    requisition.setStatusChanges(singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));
    requisition.setEmergency(true);
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitionToCopy.getFacilityId(),
        requisitionToCopy.getProgramId(),
        requisitionToCopy.getCreatedDate().toLocalDate(),
        requisitionToCopy.getCreatedDate().toLocalDate(),
        requisitionToCopy.getProcessingPeriodId(),
        requisitionToCopy.getSupervisoryNodeId(),
        EnumSet.of(requisitionToCopy.getStatus()),
        requisitionToCopy.getEmergency(),
        userPermissionStrings,
        pageRequest).getContent();

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
    requisition.setStatusChanges(singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(),
        null, null, null, null, null, null, userPermissionStrings, pageRequest).getContent();

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
        null, null, null, null, null, null, null, null, userPermissionStrings, pageRequest)
        .getContent();

    assertEquals(5, receivedRequisitions.size());
  }

  @Test
  public void testSearchEmergencyRequsitions() {
    List<Requisition> emergency = repository.searchRequisitions(
        null, null, null, null, null, null, null, true, userPermissionStrings, pageRequest)
        .getContent();

    assertEquals(2, emergency.size());
    emergency.forEach(requisition -> assertTrue(requisition.getEmergency()));
  }

  @Test
  public void testSearchStandardRequisitions() {
    List<Requisition> standard = repository.searchRequisitions(
        null, null, null, null, null, null, null, false, userPermissionStrings, pageRequest)
        .getContent();

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
  public void searchShouldExcludeRequisitionsWithNoMatchingPermissionStrings() {
    // given
    List<String> userPermissionStringSubset = singletonList(
        userPermissionStrings.get(0));

    // when
    List<Requisition> requisitions = repository.searchRequisitions(
        null, null, null, null, null, null, null, false, userPermissionStringSubset, pageRequest)
        .getContent();

    // then
    assertEquals(1, requisitions.size());
  }

  @Test
  public void testSearchRequisitionsByTemplate() {
    // given
    RequisitionTemplate nonMatchingTemplate = templateRepository
        .save(new RequisitionTemplateDataBuilder().build());
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
    UUID programId = UUID.randomUUID();
    Money pricePerPack = Money.of(CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE), 14.57);

    ProgramDto program = new ProgramDto();
    program.setId(programId);

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

    Requisition requisition = new Requisition(UUID.randomUUID(), programId,
        UUID.randomUUID(), INITIATED, false);
    requisition.initiate(setUpTemplateWithBeginningBalance(), singleton(ftap),
        Collections.emptyList(), 0, null, emptyMap(), UUID.randomUUID(), new StockData(), null);

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
  
  @Test
  public void searchByProgramSupervisoryNodePairsShouldFindIfIdsAndStatusMatch() {
    // given
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();

    Requisition matchingRequisition1 = requisitions.get(0);
    matchingRequisition1.setProgramId(programId);
    matchingRequisition1.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition1.setStatus(RequisitionStatus.AUTHORIZED);
    repository.save(matchingRequisition1);

    Requisition matchingRequisition2 = requisitions.get(1);
    matchingRequisition2.setProgramId(programId);
    matchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition2.setStatus(RequisitionStatus.IN_APPROVAL);
    repository.save(matchingRequisition2);

    Set<Pair> programNodePairs = Sets.newHashSet(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, pageRequest);
    
    // then
    assertEquals(2, results.getTotalElements());
  }
  
  @Test
  public void searchByProgramSupervisoryNodePairsShouldNotFindIfIdsDoNotMatch() {
    // given
    UUID programId = UUID.randomUUID();
    Requisition partialMatchingRequisition1 = requisitions.get(0);
    partialMatchingRequisition1.setProgramId(programId);
    partialMatchingRequisition1.setStatus(RequisitionStatus.AUTHORIZED);
    repository.save(partialMatchingRequisition1);

    UUID supervisoryNodeId = UUID.randomUUID();
    Requisition partialMatchingRequisition2 = requisitions.get(1);
    partialMatchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    partialMatchingRequisition2.setStatus(RequisitionStatus.IN_APPROVAL);
    repository.save(partialMatchingRequisition2);

    Set<Pair> programNodePairs = Sets.newHashSet(new ImmutablePair<>(programId, supervisoryNodeId));
    
    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, pageRequest);
    
    // then
    assertEquals(0, results.getTotalElements());
  }
  
  @Test
  public void searchByProgramSupervisoryNodePairsShouldNotFindIfStatusDoesNotMatch() {
    // given
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();

    for (Requisition partialMatchingRequisition : requisitions) {
      partialMatchingRequisition.setProgramId(programId);
      partialMatchingRequisition.setSupervisoryNodeId(supervisoryNodeId);
    }
    requisitions.get(0).setStatus(INITIATED);
    requisitions.get(1).setStatus(SUBMITTED);
    requisitions.get(2).setStatus(APPROVED);
    requisitions.get(3).setStatus(RELEASED);
    requisitions.get(4).setStatus(SKIPPED);
    repository.save(requisitions);
    
    Set<Pair> programNodePairs = Sets.newHashSet(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, pageRequest);

    // then
    assertEquals(0, results.getTotalElements());
  }

  @Test
  public void searchShouldUseSortProperties() {
    Requisition requisitionToCopy = requisitions.get(1);

    pageRequest = new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION,
        Sort.Direction.ASC, "createdDate");

    Requisition requisition = new Requisition(requisitionToCopy.getFacilityId(),
        requisitionToCopy.getProgramId(), requisitionToCopy.getProcessingPeriodId(),
        requisitionToCopy.getStatus(), requisitionToCopy.getEmergency());
    requisition.setSupervisoryNodeId(requisitionToCopy.getSupervisoryNodeId());
    requisition.setTemplate(testTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setStatusChanges(singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));
    requisition.setEmergency(true);
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitionToCopy.getFacilityId(),
        requisitionToCopy.getProgramId(),
        null,
        null,
        requisitionToCopy.getProcessingPeriodId(),
        requisitionToCopy.getSupervisoryNodeId(),
        EnumSet.of(requisitionToCopy.getStatus()),
        requisitionToCopy.getEmergency(),
        userPermissionStrings,
        pageRequest).getContent();

    assertEquals(2, receivedRequisitions.size());
    assertTrue(receivedRequisitions.get(0).getCreatedDate()
        .compareTo(receivedRequisitions.get(1).getCreatedDate()) < 0);

    pageRequest = new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION,
        Sort.Direction.DESC, "createdDate");

    receivedRequisitions = repository.searchRequisitions(
        requisitionToCopy.getFacilityId(),
        requisitionToCopy.getProgramId(),
        null,
        null,
        requisitionToCopy.getProcessingPeriodId(),
        requisitionToCopy.getSupervisoryNodeId(),
        EnumSet.of(requisitionToCopy.getStatus()),
        requisitionToCopy.getEmergency(),
        userPermissionStrings,
        pageRequest).getContent();

    assertEquals(2, receivedRequisitions.size());
    assertTrue(receivedRequisitions.get(0).getCreatedDate()
        .compareTo(receivedRequisitions.get(1).getCreatedDate()) > 0);
  }

  @Test(expected = PersistenceException.class)
  public void shouldNotAllowMultipleReasonsOfTheSameTypeInSingleLineItem() {
    UUID reasonId = UUID.randomUUID();

    StockAdjustment adjustment1 = new StockAdjustment();
    adjustment1.setReasonId(reasonId);
    adjustment1.setQuantity(2);

    StockAdjustment adjustment2 = new StockAdjustment();
    adjustment2.setReasonId(reasonId);
    adjustment2.setQuantity(5);

    Requisition requisition = generateInstance();
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setStockAdjustments(Lists.newArrayList(adjustment1, adjustment2));
    requisition.setRequisitionLineItems(Lists.newArrayList(lineItem));

    repository.save(requisition);

    entityManager.flush();
  }

  @Test
  public void shouldGetAllApprovedRequisitions() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    Requisition requisition2 = generateRequisition(RequisitionStatus.APPROVED);

    List<Requisition> requisitions = repository.searchApprovedRequisitions(null, null, null);

    assertEquals(2, requisitions.size());
    for (Requisition r : requisitions) {
      assertTrue(r.getId().equals(requisition1.getId())
          || r.getId().equals(requisition2.getId()));
      assertNotNull(r.getEmergency());
      assertNotNull(r.getFacilityId());
      assertNotNull(r.getProgramId());
      assertNotNull(r.getProcessingPeriodId());
      assertNotNull(r.getStatusChanges());
    }
  }

  @Test
  public void shouldFilterApprovedRequisitionsByAnyField() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    List<Requisition> requisitions = repository.searchApprovedRequisitions("all",
        singletonList(requisition1.getFacilityId()),
        singletonList(requisition1.getProgramId()));

    assertEquals(1, requisitions.size());
    assertTrue(requisitions.get(0).getId().equals(requisition1.getId()));
  }

  @Test
  public void shouldFilterApprovedRequisitionsByFacility() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    List<Requisition> requisitions = repository.searchApprovedRequisitions("facilityName",
        Lists.newArrayList(requisition1.getFacilityId()), null);

    assertEquals(1, requisitions.size());
    assertTrue(requisitions.get(0).getId().equals(requisition1.getId()));
  }

  @Test
  public void shouldFilterApprovedRequisitionsByProgram() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    List<Requisition> requisitions = repository.searchApprovedRequisitions("programName",
        null, Lists.newArrayList(requisition1.getProgramId()));

    assertEquals(1, requisitions.size());
    assertTrue(requisitions.get(0).getId().equals(requisition1.getId()));
  }

  @Test
  public void shouldFilterApprovedRequisitionsByMultipleValues() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    Requisition requisition2 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    List<Requisition> requisitions = repository.searchApprovedRequisitions("facilityName",
        Lists.newArrayList(requisition1.getFacilityId(), requisition2.getFacilityId()), null);

    assertEquals(2, requisitions.size());
    List<UUID> requisitionIds = requisitions.stream()
        .map(Requisition::getId)
        .collect(Collectors.toList());
    assertTrue(requisitionIds.contains(requisition1.getId()));
    assertTrue(requisitionIds.contains(requisition2.getId()));
  }

  @Test
  public void shouldReturnEmptyListOnEmptyFiltersWhenFilteringByFields() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    List<Requisition> requisitions = repository.searchApprovedRequisitions("facilityName",
        Collections.emptyList(), Collections.emptyList());

    assertNotNull(requisitions);
    assertTrue(requisitions.isEmpty());
  }

  @Test
  public void shouldReadById() {
    List<Requisition> requisitions = repository.readDistinctByIdIn(
        Arrays.asList(this.requisitions.get(0).getId(), this.requisitions.get(3).getId()));

    assertEquals(2, requisitions.size());
  }

  @Test
  public void shouldReadDistinctById() {
    Requisition requisition = generateInstance();
    RequisitionLineItem item = generateLineItem(requisition);
    RequisitionLineItem item2 = generateLineItem(requisition);
    requisition.setRequisitionLineItems(asList(item, item2));
    requisition = repository.save(requisition);

    List<Requisition> found = repository.readDistinctByIdIn(singletonList(requisition.getId()));

    assertEquals(1, found.size());
  }

  private RequisitionLineItem generateLineItem(Requisition requisition) {
    RequisitionLineItem item = new RequisitionLineItem();
    item.setRequisition(requisition);
    return item;
  }

  private RequisitionTemplate setUpTemplateWithBeginningBalance() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(RequisitionLineItem.BEGINNING_BALANCE)
        .build();

    return templateRepository.save(new RequisitionTemplate(
        Collections.singletonMap(RequisitionLineItem.BEGINNING_BALANCE, column)));
  }

  private Requisition generateRequisition(RequisitionStatus status) {
    return generateRequisition(status, UUID.randomUUID(), UUID.randomUUID());
  }

  private Requisition generateRequisition(RequisitionStatus status, UUID facility, UUID program) {
    Requisition requisition = generateInstance();
    requisition.setStatus(status);
    requisition.setFacilityId(facility);
    requisition.setProgramId(program);

    requisition.setStatusChanges(singletonList(
        StatusChange.newStatusChange(requisition, UUID.randomUUID())));

    repository.save(requisition);

    return requisition;
  }
}
