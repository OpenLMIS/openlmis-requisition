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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Set;

import org.junit.Test;
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
import org.openlmis.requisition.testutils.RejectionReasonDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

public class RejectionReasonRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RejectionReason> {

  @Autowired
  private RejectionReasonRepository repository;

  @Autowired
  private RejectionReasonCategoryRepository rejectionReasonCategoryRepository;

  private Pageable pageable;

  private RejectionReasonCategory rejectionReasonCategory;

  private RejectionReason rejectionReason;

  private static final String REJECTION_REASON_NAME = "Reason Name";
  private static final String REJECTION_REASON_CODE = "RR1";
  private static final String REJECTION_REASON_CATEGORY_NAME = "name";
  private static final String REJECTION_REASON_CATEGORY_CODE = "RRC1";

  @Override
  RejectionReasonRepository getRepository() {
    return this.repository;
  }

  @Override
  RejectionReason generateInstance() {
    pageable = PageRequest.of(0, 10);
    RejectionReasonCategory rejectionReasonCategory = new RejectionReasonCategoryDataBuilder()
            .withName("name")
            .withCode("code")
            .withActive(true)
            .buildAsNew();
    rejectionReasonCategoryRepository.save(rejectionReasonCategory);

    return new RejectionReasonDataBuilder()
            .withCategory(rejectionReasonCategory)
            .withActive(true)
            .withCode("code")
            .withName("name")
            .buildAsNew();
  }

  private RejectionReason generateRejectionReason(RejectionReasonCategory rejectionReasonCategory) {
    return new RejectionReasonDataBuilder()
            .withCategory(rejectionReasonCategory)
            .withName(REJECTION_REASON_NAME)
            .withCode(REJECTION_REASON_CODE)
            .withActive(true)
            .buildAsNew();
  }

  private RejectionReasonCategory generateRejectionReasonCategory() {
    return new RejectionReasonCategoryDataBuilder()
            .withName(REJECTION_REASON_CATEGORY_NAME)
            .withCode(REJECTION_REASON_CATEGORY_CODE)
            .buildAsNew();
  }

  @Test
  public void shouldFindRejectionReasonByName() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Set<RejectionReason> rejectionReasons = repository.searchRejectionReason(
            REJECTION_REASON_NAME,
            null, null);
    assertEquals(1, rejectionReasons.size());
    assertTrue(rejectionReasons.stream().allMatch(result -> rejectionReasons
            .contains(rejectionReason)));
  }

  @Test
  public void shouldFindRejectionReasonByCode() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Set<RejectionReason> rejectionReasons = repository.searchRejectionReason(
            null, REJECTION_REASON_CODE, null);
    assertEquals(1, rejectionReasons.size());
    assertTrue(rejectionReasons.stream().allMatch(result -> rejectionReasons
            .contains(rejectionReason)));
  }

  @Test
  public void shouldFindRejectionReasonByCategory() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Set<RejectionReason> rejectionReasons = repository.searchRejectionReason(
            null, null, rejectionReasonCategory);
    assertEquals(1, rejectionReasons.size());
    assertTrue(rejectionReasons.stream().allMatch(result -> rejectionReasons
            .contains(rejectionReason)));
  }

  @Test
  public void shouldFindRejectionReasonByNameAndCodeAndCategory() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Set<RejectionReason> rejectionReasons = repository.searchRejectionReason(
            REJECTION_REASON_NAME, REJECTION_REASON_CODE,
            rejectionReasonCategory);
    assertEquals(1, rejectionReasons.size());
    assertTrue(rejectionReasons.stream().allMatch(result -> rejectionReasons
            .contains(rejectionReason)));
  }

  @Test
  public void shouldFindActiveRejectionReason() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Set<RejectionReason> rejectionReasons = repository.findByActive(true);
    assertEquals(1, rejectionReasons.size());
    assertTrue(rejectionReasons.stream().allMatch(result -> rejectionReasons
            .contains(rejectionReason)));
  }

  @Test
  public void shouldReturnAllRejectionReasonsIFallParameterAreNull() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Set<RejectionReason> rejectionReasons = repository.searchRejectionReason(null,
            null,
            null);
    assertEquals(1, rejectionReasons.size());
    assertTrue(rejectionReasons.stream().allMatch(result -> rejectionReasons
            .contains(rejectionReason)));
  }

  @Test
  public void shouldReturnAllRejectionReason() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = rejectionReasonCategoryRepository.save(rejectionReasonCategory);
    rejectionReason = generateRejectionReason(rejectionReasonCategory);
    repository.save(rejectionReason);

    Page rejectionReasons = repository.findAllWithoutSnapshots(pageable);

    assertEquals(1, rejectionReasons.getContent().size());
    assertEquals(1, rejectionReasons.getContent().size());
  }

}
