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
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isIn;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.IN_APPROVAL;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Sets;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import javax.persistence.PersistenceException;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionLineItemDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionPeriod;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StockAdjustment;
import org.openlmis.requisition.domain.requisition.StockAdjustmentDataBuilder;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.repository.custom.DefaultRequisitionSearchParams;
import org.openlmis.requisition.repository.custom.RequisitionSearchParams;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.openlmis.requisition.testutils.DefaultRequisitionSearchParamsDataBuilder;
import org.openlmis.requisition.testutils.StatusChangeDataBuilder;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseRequisitionRepositoryIntegrationTest {

  private List<Requisition> requisitions;

  private Pageable pageRequest = PageRequest.of(
      Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION);

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

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

    Requisition requisition = new RequisitionDataBuilder()
        .withFacilityId(requisitionToCopy.getFacilityId())
        .withProgramId(requisitionToCopy.getProgramId())
        .withProcessingPeriodId(requisitionToCopy.getProcessingPeriodId())
        .withStatus(requisitionToCopy.getStatus())
        .withModifiedDate(requisitionToCopy.getModifiedDate())
        .withSupervisoryNodeId(requisitionToCopy.getSupervisoryNodeId())
        .withTemplate(testTemplate)
        .withNumberOfMonthsInPeriod(1)
        .withEmergency(true)
        .buildAsNew();
    requisition.setStatusChanges(
        singletonList(new StatusChangeDataBuilder()
            .withRequisition(requisition)
            .buildAsNew()
        )
    );
    repository.save(requisition);

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withFacility(requisitionToCopy.getFacilityId())
        .withProgram(requisitionToCopy.getProgramId())
        .withProcessingPeriod(requisitionToCopy.getProcessingPeriodId())
        .withSupervisoryNode(requisitionToCopy.getSupervisoryNodeId())
        .withEmergency(requisitionToCopy.getEmergency())
        .withInitiatedDateFrom(requisitionToCopy.getCreatedDate().toLocalDate())
        .withInitiatedDateTo(requisitionToCopy.getCreatedDate().toLocalDate())
        .withModifiedDateFrom(requisitionToCopy.getModifiedDate())
        .withModifiedDateTo(requisitionToCopy.getModifiedDate())
        .withRequisitionStatuses(EnumSet.of(requisitionToCopy.getStatus()))
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

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
      assertEquals(receivedRequisition.getModifiedDate(), receivedRequisition.getCreatedDate());
      assertEquals(receivedRequisition.getNumberOfMonthsInPeriod(), Integer.valueOf(1));
      assertEquals(receivedRequisition.getEmergency(), requisitionToCopy.getEmergency());
    }
  }

  @Test
  public void testSearchRequisitionsByModifiedDateFrom() {

    Requisition requisition1 = requisitions.get(0);
    Requisition requisition2 = requisitions.get(1);
    Requisition requisition3 = requisitions.get(2);
    requisition1.setModifiedDate(requisition1.getModifiedDate().plusMonths(1));
    requisition2.setModifiedDate(requisition2.getModifiedDate().plusMonths(2));
    requisition3.setModifiedDate(requisition3.getModifiedDate().plusMonths(3));

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withModifiedDateFrom(requisition1.getModifiedDate())
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(3, receivedRequisitions.size());
    assertEquals(receivedRequisitions.get(0).getModifiedDate(), requisition1.getModifiedDate());
    assertEquals(receivedRequisitions.get(1).getModifiedDate(), requisition2.getModifiedDate());
    assertEquals(receivedRequisitions.get(2).getModifiedDate(), requisition3.getModifiedDate());
  }

  @Test
  public void testSearchRequisitionsByModifiedDateTo() {

    Requisition requisition1 = requisitions.get(0);
    Requisition requisition2 = requisitions.get(1);
    Requisition requisition3 = requisitions.get(2);

    requisition1.setModifiedDate(requisition1.getModifiedDate().minusMonths(3));
    requisition2.setModifiedDate(requisition2.getModifiedDate().minusMonths(2));
    requisition3.setModifiedDate(requisition3.getModifiedDate().minusMonths(1));

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withModifiedDateTo(requisition3.getModifiedDate())
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(3, receivedRequisitions.size());
    assertEquals(receivedRequisitions.get(0).getModifiedDate(), requisition1.getModifiedDate());
    assertEquals(receivedRequisitions.get(1).getModifiedDate(), requisition2.getModifiedDate());
    assertEquals(receivedRequisitions.get(2).getModifiedDate(), requisition3.getModifiedDate());
  }

  @Test
  public void testSearchRequisitionsByStartAndEndModifiedDate() {

    Requisition requisition1 = requisitions.get(0);
    Requisition requisition2 = requisitions.get(1);
    Requisition requisition3 = requisitions.get(2);
    requisition1.setModifiedDate(requisition1.getModifiedDate().minusMonths(3));
    requisition2.setModifiedDate(requisition2.getModifiedDate().minusMonths(2));
    requisition3.setModifiedDate(requisition3.getModifiedDate().minusMonths(1));

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withModifiedDateFrom(requisition2.getModifiedDate())
        .withModifiedDateTo(requisition3.getModifiedDate())
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(2, receivedRequisitions.size());
    assertEquals(receivedRequisitions.get(0).getModifiedDate(), requisition2.getModifiedDate());
    assertEquals(receivedRequisitions.get(1).getModifiedDate(), requisition3.getModifiedDate());
  }

  @Test
  public void testSearchRequisitionsByFacilityAndProgram() {
    Requisition requisitionToCopy = requisitions.get(0);

    Requisition requisition = new RequisitionDataBuilder()
        .withFacilityId(requisitionToCopy.getFacilityId())
        .withProgramId(requisitionToCopy.getProgramId())
        .withStatus(requisitionToCopy.getStatus())
        .withSupervisoryNodeId(requisitionToCopy.getSupervisoryNodeId())
        .withTemplate(testTemplate)
        .withNumberOfMonthsInPeriod(1)
        .withEmergency(false)
        .buildAsNew();

    requisition.setStatusChanges(
        singletonList(new StatusChangeDataBuilder()
            .withRequisition(requisition)
            .buildAsNew()
        )
    );
    repository.save(requisition);

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withFacility(requisitions.get(0).getFacilityId())
        .withProgram(requisitions.get(0).getProgramId())
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

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
    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParams();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(5, receivedRequisitions.size());
  }

  @Test
  public void testSearchEmergencyRequsitions() {
    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withEmergency(true)
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(2, receivedRequisitions.size());
    receivedRequisitions.forEach(requisition -> assertTrue(requisition.getEmergency()));
  }

  @Test
  public void testSearchStandardRequisitions() {
    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .withEmergency(false)
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(3, receivedRequisitions.size());
    receivedRequisitions.forEach(requisition -> assertFalse(requisition.getEmergency()));
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
  public void testSearchRequisitionIdStatusPairsByPeriodAndEmergencyFlag() {
    requisitions.forEach(requisition -> {
      List<RequisitionPeriod> found = repository.searchRequisitionIdAndStatusPairs(
          requisition.getFacilityId(), requisition.getProgramId(), requisition.getEmergency()
      );

      found.forEach(element -> {
        assertEquals(requisition.getStatus(), element.getRequisitionStatus());
        assertEquals(requisition.getId(), element.getRequisitionId());
      });
    });
  }

  @Test
  public void searchShouldExcludeRequisitionsWithNoMatchingPermissionStrings() {
    // given
    List<String> userPermissionStringSubset = singletonList(
        userPermissionStrings.get(0));

    // when
    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStringSubset, emptySet(), pageRequest)
        .getContent();

    // then
    assertEquals(1, receivedRequisitions.size());
    assertThat(
        receivedRequisitions.get(0).getPermissionStrings().get(0).getPermissionString(),
        isIn(userPermissionStringSubset));
  }

  @Test
  public void searchShouldExcludeRequisitionsWithNoMatchingProgramNodePair() {
    // given
    Set<Pair<UUID, UUID>> programNodePairsSubset = singleton(programNodePairs.iterator().next());

    // when
    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .build();

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, emptyList(), programNodePairsSubset, pageRequest)
        .getContent();

    // then
    assertEquals(1, receivedRequisitions.size());

    ImmutablePair<UUID, UUID> receivedPair = new ImmutablePair<>(
        receivedRequisitions.get(0).getProgramId(),
        receivedRequisitions.get(0).getSupervisoryNodeId());

    assertThat(receivedPair, isIn(programNodePairsSubset));
  }

  @Test
  public void searchShouldNotReturnDuplicationsIfPermissionStringsAndProgramNodePairsAreSame() {
    // given
    Requisition requisition = requisitions.get(1);

    List<String> permissionStringsSubset = Lists.newArrayList();
    permissionStringsSubset.add(String.format(
        "REQUISITION_VIEW|%s|%s", requisition.getFacilityId(), requisition.getProgramId()));

    Set<Pair<UUID, UUID>> programNodePairSubset = Sets.newHashSet();
    programNodePairSubset.add(
        new ImmutablePair<>(requisition.getProgramId(), requisition.getSupervisoryNodeId()));

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .build();

    List<Requisition> found = repository
        .searchRequisitions(searchParams, permissionStringsSubset,
            programNodePairSubset, pageRequest)
        .getContent();

    assertThat(found, hasSize(1));
    assertThat(found, hasItem(requisition));
  }

  @Test
  public void searchShouldCombineResultsIfPermissionStringsAndProgramNodePairsAreDifferent() {
    // given
    Requisition firstRnR = requisitions.get(1);
    Requisition secondRnR = requisitions.get(4);

    List<String> permissionStringsSubset = Lists.newArrayList();
    permissionStringsSubset.add(String.format(
        "REQUISITION_VIEW|%s|%s", firstRnR.getFacilityId(), firstRnR.getProgramId()));

    Set<Pair<UUID, UUID>> programNodePairSubset = Sets.newHashSet();
    programNodePairSubset.add(
        new ImmutablePair<>(secondRnR.getProgramId(), secondRnR.getSupervisoryNodeId()));

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParamsDataBuilder()
        .build();

    List<Requisition> found = repository
        .searchRequisitions(searchParams, permissionStringsSubset,
            programNodePairSubset, pageRequest)
        .getContent();

    assertThat(found, hasSize(2));
    assertThat(found, hasItem(firstRnR));
    assertThat(found, hasItem(secondRnR));
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

    requisitions.forEach(repository::save);

    // when
    List<Requisition> result = repository.findByTemplateId(testTemplate.getId());

    // then
    assertEquals(matchingRequisitions.size(), result.size());
  }

  @Test
  public void shouldFindRequisitionsByMultipleIds() {
    UUID id1 = requisitions.get(0).getId();
    UUID id2 = requisitions.get(1).getId();

    List<Requisition> foundRequisitions = Lists.newArrayList(repository.findAllById(
        Lists.newArrayList(id1, id2, UUID.randomUUID())));

    assertThat(foundRequisitions, is(notNullValue()));
    assertThat(foundRequisitions, hasSize(2));
    assertThat(foundRequisitions, hasItem(hasProperty("id", is(id1))));
    assertThat(foundRequisitions, hasItem(hasProperty("id", is(id2))));
  }

  @Test
  public void shouldPersistWithPreviousRequisitions() {
    Requisition requisition = new RequisitionDataBuilder()
        .buildAsNew();
    requisition.setPreviousRequisitions(requisitions);

    requisition = repository.save(requisition);
    requisition = repository.findById(requisition.getId()).orElse(null);

    assertEquals(5, requisition.getPreviousRequisitions().size());
  }

  @Test(expected = PersistenceException.class)
  public void shouldNotAllowMultipleRegularRequisitionForFacilityProgramPeriod() {
    shouldNotAllowMultipleRegularRequisition(null);
  }

  @Test(expected = PersistenceException.class)
  public void shouldNotAllowMultipleRegularRequisitionForFacilityProgramPeriodNode() {
    shouldNotAllowMultipleRegularRequisition(UUID.randomUUID());
  }

  private void shouldNotAllowMultipleRegularRequisition(UUID supervisoryNodeId) {
    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID periodId = UUID.randomUUID();

    Requisition requisition1 = generateInstance(facilityId, programId, periodId);
    requisition1.setSupervisoryNodeId(supervisoryNodeId);

    Requisition requisition2 = generateInstance(facilityId, programId, periodId);
    requisition2.setSupervisoryNodeId(supervisoryNodeId);

    requisition1.setEmergency(false);
    requisition2.setEmergency(false);

    repository.saveAll(asList(requisition1, requisition2));

    entityManager.flush();
  }

  @Test
  public void shouldAllowMultipleEmergencyRequisitionsForFacilityProgramPeriod() {
    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID periodId = UUID.randomUUID();

    Requisition regularRequisition = generateInstance(facilityId, programId, periodId);
    Requisition emergencyRequisition1 = generateInstance(facilityId, programId, periodId);
    Requisition emergencyRequisition2 = generateInstance(facilityId, programId, periodId);

    emergencyRequisition1.setEmergency(true);
    emergencyRequisition2.setEmergency(true);
    regularRequisition.setEmergency(false);

    repository.saveAll(asList(regularRequisition, emergencyRequisition1, emergencyRequisition2));

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

    // simulation that the requisition has been rejected 3 times
    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    Requisition matchingRequisition2 = requisitions.get(1);
    matchingRequisition2.setProgramId(programId);
    matchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition2.setStatus(RequisitionStatus.IN_APPROVAL);

    // simulation that the requisition has been rejected 2 times
    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    repository.save(matchingRequisition2);

    Set<Pair<UUID, UUID>> programNodePairs =
        singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, null, null,
            pageRequest);

    // then
    assertEquals(2, results.getTotalElements());
  }

  @Test
  public void searchByProgramSupervisoryNodePairsShouldFindIfIdsAndStatusAndFacilityMatch() {
    // given
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();
    UUID facilityId = UUID.randomUUID();

    Requisition matchingRequisition1 = requisitions.get(0);
    matchingRequisition1.setProgramId(programId);
    matchingRequisition1.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition1.setStatus(RequisitionStatus.AUTHORIZED);
    matchingRequisition1.setFacilityId(facilityId);

    // simulation that the requisition has been rejected 3 times
    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    //Set<Pair<UUID, UUID>> programNodePairs =
    //singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(new HashSet<>(), facilityId,
            null, pageRequest);

    // then
    assertEquals(1, results.getTotalElements());
  }

  @Test
  public void searchByProgramSupervisoryNodePairsShouldSortEmergencyRequisitionsFirst() {
    // given
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();

    Requisition matchingRequisition1 = requisitions.get(0);
    matchingRequisition1.setProgramId(programId);
    matchingRequisition1.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition1.setStatus(RequisitionStatus.AUTHORIZED);
    matchingRequisition1.setEmergency(false);
    matchingRequisition1.getStatusChanges().clear();

    // simulation that the requisition has been rejected 1 times
    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    Requisition matchingRequisition2 = requisitions.get(1);
    matchingRequisition2.setProgramId(programId);
    matchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition2.setStatus(RequisitionStatus.IN_APPROVAL);
    matchingRequisition2.setEmergency(true);
    matchingRequisition2.getStatusChanges().clear();

    // simulation that the requisition has not been rejected
    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    Set<Pair<UUID, UUID>> programNodePairs =
        singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    Pageable sortPageRequest = PageRequest.of(
        Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION, Direction.DESC, "emergency");

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(
            programNodePairs, null, null, sortPageRequest);

    // then
    assertEquals(2, results.getTotalElements());
    assertThat(results.getContent().get(0), is(matchingRequisition2));
    assertThat(results.getContent().get(1), is(matchingRequisition1));
  }

  @Test
  public void searchByProgramSupervisoryNodePairsShouldSortByLatestAuthorized() {
    searchByProgramSupervisoryNodePairsShouldSortByAuthorizedDate(Direction.DESC);
  }

  @Test
  public void searchByProgramSupervisoryNodePairsShouldSortByFirstAuthorized() {
    searchByProgramSupervisoryNodePairsShouldSortByAuthorizedDate(Direction.ASC);
  }

  private void searchByProgramSupervisoryNodePairsShouldSortByAuthorizedDate(Direction direction) {
    // given
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();
    final UUID user = UUID.randomUUID();
    final Map<VersionIdentityDto, OrderableDto> products = emptyMap();

    Requisition matchingRequisition1 = requisitions.get(0);
    matchingRequisition1.setProgramId(programId);
    matchingRequisition1.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition1.setEmergency(false);

    Requisition matchingRequisition2 = requisitions.get(1);
    matchingRequisition2.setProgramId(programId);
    matchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition2.setEmergency(true);

    Requisition matchingRequisition3 = requisitions.get(2);
    matchingRequisition3.setProgramId(programId);
    matchingRequisition3.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition3.setEmergency(true);

    repository.save(matchingRequisition1);
    repository.save(matchingRequisition2);
    repository.save(matchingRequisition3);

    // 1) first authorize for matchingRequisition2 have to be before matchingRequisition3
    //    but second authorize for matchingRequisition2 have to be after matchingRequisition3
    //    to verify that the latest authorized status change is used for comparison
    // 2) we have to save requisitions after each status change because the createdDate field
    //    is set by hibernate - because of @PrePersist annotation in the BaseTimestampedEntity
    matchingRequisition1.submit(products, user, false);
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition2.submit(products, user, false);
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition3.submit(products, user, false);
    saveAndFlushWithDelay(matchingRequisition3);

    matchingRequisition1.authorize(products, user);
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition2.authorize(products, user);
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition3.authorize(products, user);
    saveAndFlushWithDelay(matchingRequisition3);

    matchingRequisition2.reject(products, user);
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition3.reject(products, user);
    saveAndFlushWithDelay(matchingRequisition3);

    matchingRequisition3.submit(products, user, false);
    saveAndFlushWithDelay(matchingRequisition3);

    matchingRequisition2.submit(products, user, false);
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition3.authorize(products, user);
    matchingRequisition3.setSupervisoryNodeId(supervisoryNodeId);
    saveAndFlushWithDelay(matchingRequisition3);

    matchingRequisition2.authorize(products, user);
    matchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    saveAndFlushWithDelay(matchingRequisition2);

    Set<Pair<UUID, UUID>> programNodePairs =
        singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    Pageable sortPageRequest = PageRequest.of(
        Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION, direction, "authorizedDate");

    // make sure that data have been retrieved from the database
    entityManager.clear();

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(
            programNodePairs, null, null, sortPageRequest);

    // then
    assertEquals(3, results.getTotalElements());

    if (direction == Direction.ASC) {
      assertRequisitionAfterSortByAuthorizedDate(results.getContent().get(0), matchingRequisition1);
      assertRequisitionAfterSortByAuthorizedDate(results.getContent().get(1), matchingRequisition3);
      assertRequisitionAfterSortByAuthorizedDate(results.getContent().get(2), matchingRequisition2);
    } else {
      assertRequisitionAfterSortByAuthorizedDate(results.getContent().get(0), matchingRequisition2);
      assertRequisitionAfterSortByAuthorizedDate(results.getContent().get(1), matchingRequisition3);
      assertRequisitionAfterSortByAuthorizedDate(results.getContent().get(2), matchingRequisition1);
    }
  }

  private void assertRequisitionAfterSortByAuthorizedDate(Requisition actual,
      Requisition expected) {

    assertThat(actual, allOf(
        hasProperty("id", is(expected.getId())),
        hasProperty("statusChanges", hasSize(expected.getStatusChanges().size()))));
  }

  private void saveAndFlushWithDelay(Requisition requisition) {
    repository.saveAndFlush(requisition);

    try {
      TimeUnit.MILLISECONDS.sleep(10);
    } catch (InterruptedException exp) {
      Thread.currentThread().interrupt();
      throw new IllegalStateException(exp);
    }
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

    Set<Pair<UUID, UUID>> programNodePairs =
        singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, null, null,
            pageRequest);

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
    repository.saveAll(requisitions);

    Set<Pair<UUID, UUID>> programNodePairs =
        singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, null, null,
            pageRequest);

    // then
    assertEquals(0, results.getTotalElements());
  }

  @Test
  public void searchByProgramSupervisoryNodePairsShouldFindPartnerRequisitionsReadyForApproval() {
    // given
    Requisition partnerRequisition = requisitions.get(0);
    partnerRequisition.setStatus(IN_APPROVAL);
    partnerRequisition.setVersion(1L);
    partnerRequisition.setDraftStatusMessage("");
    partnerRequisition.getStatusChanges().clear();
    partnerRequisition.setSupervisoryNodeId(UUID.randomUUID());

    repository.save(partnerRequisition);

    Set<Pair<UUID, UUID>> programNodePairs = singleton(new ImmutablePair<>(
        partnerRequisition.getProgramId(), partnerRequisition.getSupervisoryNodeId()));

    // when
    Page<Requisition> results = repository
        .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, null, null,
            pageRequest);

    // then
    assertThat(results.getContent(), hasSize(1));
    assertThat(results.getContent().get(0).getId(), is(partnerRequisition.getId()));
  }

  @Test
  public void searchShouldUseSortProperties() {
    Requisition requisitionToCopy = requisitions.get(1);

    pageRequest = PageRequest.of(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION,
        Sort.Direction.ASC, "createdDate");

    Requisition requisition = new RequisitionDataBuilder()
        .withFacilityId(requisitionToCopy.getFacilityId())
        .withProgramId(requisitionToCopy.getProgramId())
        .withProcessingPeriodId(requisitionToCopy.getProcessingPeriodId())
        .withStatus(requisitionToCopy.getStatus())
        .withEmergency(requisitionToCopy.getEmergency())
        .withSupervisoryNodeId(requisitionToCopy.getSupervisoryNodeId())
        .withTemplate(testTemplate)
        .withNumberOfMonthsInPeriod(1)
        .withEmergency(true)
        .buildAsNew();

    requisition.setStatusChanges(
        singletonList(new StatusChangeDataBuilder()
            .withRequisition(requisition)
            .buildAsNew()
        )
    );
    repository.save(requisition);

    RequisitionSearchParams searchParams = new DefaultRequisitionSearchParams(
        requisitionToCopy.getFacilityId(), requisitionToCopy.getProgramId(),
        requisitionToCopy.getProcessingPeriodId(), requisitionToCopy.getSupervisoryNodeId(),
        requisitionToCopy.getEmergency(), null, null, null, null,
        EnumSet.of(requisitionToCopy.getStatus()));

    List<Requisition> receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(2, receivedRequisitions.size());
    assertTrue(receivedRequisitions.get(0).getCreatedDate()
        .compareTo(receivedRequisitions.get(1).getCreatedDate()) < 0);

    pageRequest = PageRequest.of(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION,
        Sort.Direction.DESC, "createdDate");

    receivedRequisitions = repository
        .searchRequisitions(searchParams, userPermissionStrings, programNodePairs, pageRequest)
        .getContent();

    assertEquals(2, receivedRequisitions.size());
    assertTrue(receivedRequisitions.get(0).getCreatedDate()
        .compareTo(receivedRequisitions.get(1).getCreatedDate()) > 0);
  }

  @Test(expected = PersistenceException.class)
  public void shouldNotAllowMultipleReasonsOfTheSameTypeInSingleLineItem() {
    UUID reasonId = UUID.randomUUID();

    StockAdjustment adjustment1 = new StockAdjustmentDataBuilder()
        .withReasonId(reasonId)
        .withQuantity(2)
        .buildAsNew();

    StockAdjustment adjustment2 = new StockAdjustmentDataBuilder()
        .withReasonId(reasonId)
        .withQuantity(5)
        .buildAsNew();

    Requisition requisition = generateInstance();
    requisition.setId(UUID.randomUUID());
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setStockAdjustments(Lists.newArrayList(adjustment1, adjustment2));
    requisition.setRequisitionLineItems(Lists.newArrayList(lineItem));

    repository.save(requisition);

    entityManager.flush();
  }

  @Test
  public void shouldInitCreatedAndModifiedDate() {
    Requisition requisition = generateInstance();
    requisition.setCreatedDate(null);
    requisition.setModifiedDate(null);

    repository.saveAndFlush(requisition);

    assertNotNull(requisition.getCreatedDate());
    assertEquals(requisition.getCreatedDate(), requisition.getModifiedDate());
  }

  @Test
  public void shouldGetAllApprovedRequisitions() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    Requisition requisition2 = generateRequisition(RequisitionStatus.APPROVED);

    Page<Requisition> requisitions = repository.searchApprovedRequisitions(
        null, emptySet(), createPageable(10, 0));

    assertEquals(2, requisitions.getTotalElements());
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
  public void shouldFilterApprovedRequisitionsByFacilityId() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    Page<Requisition> requisitions = repository.searchApprovedRequisitions(
        requisition1.getFacilityId(),
        emptySet(),
        createPageable(10, 0));

    assertEquals(1, requisitions.getTotalElements());
    assertThat(requisitions, hasItem(hasProperty("id", is(requisition1.getId()))));
  }

  @Test
  public void shouldFilterApprovedRequisitionsByProgramId() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    Page<Requisition> requisitions = repository.searchApprovedRequisitions(
        null,
        singleton(Pair.of(requisition1.getProgramId(), null)),
        createPageable(10, 0));

    assertEquals(1, requisitions.getTotalElements());
    assertThat(requisitions, hasItem(hasProperty("id", is(requisition1.getId()))));
  }

  @Test
  public void shouldFilterApprovedRequisitionsByFacilityIdAndProgramId() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    Page<Requisition> requisitions = repository.searchApprovedRequisitions(
        requisition1.getFacilityId(),
        singleton(Pair.of(requisition1.getProgramId(), requisition1.getSupervisoryNodeId())),
        createPageable(10, 0));

    assertEquals(1, requisitions.getTotalElements());
    List<UUID> requisitionIds = requisitions
        .map(Requisition::getId)
        .getContent();
    assertTrue(requisitionIds.contains(requisition1.getId()));
  }

  @Test
  public void shouldReturnEmptyPageWhenNoRequisitionsWithGivenParametersFound() {
    Requisition requisition1 = generateRequisition(RequisitionStatus.APPROVED);
    generateRequisition(INITIATED, requisition1.getFacilityId(), requisition1.getProgramId());

    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();

    Page<Requisition> requisitions = repository.searchApprovedRequisitions(
        facilityId,
        singleton(Pair.of(programId, supervisoryNodeId)),
        createPageable(10, 0));

    assertEquals(0, requisitions.getTotalElements());
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

  @Test
  public void shouldReturnTrueIfThereAreRequisitionsWithOriginalRequisitionId() {
    // given
    UUID originalRequisitionId = requisitions.get(3).getId();

    for (int i = 0, size = requisitions.size(); i < size; ++i) {
      if (3 == i) {
        // because we use this requisition as an original requisition
        requisitions.get(i).setOriginalRequisitionId(null);
      } else {
        requisitions.get(i).setOriginalRequisitionId(originalRequisitionId);
      }
    }

    repository.saveAll(requisitions);

    // when
    boolean result = repository.existsByOriginalRequisitionId(originalRequisitionId);

    // then
    assertThat(result, is(true));
  }

  @Test
  public void shouldReturnFalseIfThereAreNoRequisitionsWithOriginalRequisitionId() {
    // given
    requisitions.forEach(requisition -> requisition.setOriginalRequisitionId(null));
    repository.saveAll(requisitions);

    // when
    boolean result = repository.existsByOriginalRequisitionId(requisitions.get(3).getId());

    // then
    assertThat(result, is(false));
  }

  @Test
  public void countByProgramSupervisoryNodePairsShouldFindIfIdsAndStatusMatch() {
    // given
    UUID programId = UUID.randomUUID();
    UUID supervisoryNodeId = UUID.randomUUID();

    Requisition matchingRequisition1 = requisitions.get(0);
    matchingRequisition1.setProgramId(programId);
    matchingRequisition1.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition1.setStatus(RequisitionStatus.AUTHORIZED);

    // simulation that the requisition has been rejected 3 times
    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    matchingRequisition1
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition1)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition1);

    Requisition matchingRequisition2 = requisitions.get(1);
    matchingRequisition2.setProgramId(programId);
    matchingRequisition2.setSupervisoryNodeId(supervisoryNodeId);
    matchingRequisition2.setStatus(RequisitionStatus.IN_APPROVAL);

    // simulation that the requisition has been rejected 2 times
    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    matchingRequisition2
        .getStatusChanges()
        .add(new StatusChangeDataBuilder()
            .forAuthorizedRequisition(matchingRequisition2)
            .buildAsNew());
    saveAndFlushWithDelay(matchingRequisition2);

    repository.save(matchingRequisition2);

    Set<Pair<UUID, UUID>> programNodePairs =
        singleton(new ImmutablePair<>(programId, supervisoryNodeId));

    // when
    long result = repository
        .countApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs);

    // then
    assertEquals(2L, result);
  }

  private RequisitionLineItem generateLineItem(Requisition requisition) {
    RequisitionLineItem item = new RequisitionLineItemDataBuilder()
        .withRequisition(requisition)
        .buildAsNew();
    return item;
  }

  private RequisitionTemplate setUpTemplateWithBeginningBalance() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(RequisitionLineItem.BEGINNING_BALANCE)
        .withoutOption()
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withoutId()
            .withoutOptions()
            .build())
        .build();

    availableRequisitionColumnRepository.save(column.getColumnDefinition());

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
