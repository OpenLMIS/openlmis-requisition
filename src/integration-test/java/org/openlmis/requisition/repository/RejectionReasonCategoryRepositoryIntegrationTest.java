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

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

public class RejectionReasonCategoryRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RejectionReasonCategory> {

  @Autowired
  private RejectionReasonCategoryRepository repository;

  private Pageable pageable;

  private RejectionReasonCategory rejectionReasonCategory;

  private static final String REJECTION_REASON_CATEGORY_NAME = "Category Name";
  private static final String REJECTION_REASON_CATEGORY_CODE = "RRC1";

  @Override
  RejectionReasonCategoryRepository getRepository() {
    return this.repository;
  }

  @Override
  RejectionReasonCategory generateInstance() {
    return new RejectionReasonCategoryDataBuilder()
            .withActive(true)
            .withCode("code")
            .withName("name")
            .buildAsNew();
  }

  private RejectionReasonCategory generateRejectionReasonCategory() {
    return new RejectionReasonCategoryDataBuilder()
            .withName(REJECTION_REASON_CATEGORY_NAME)
            .withCode(REJECTION_REASON_CATEGORY_CODE)
            .withActive(true)
            .buildAsNew();
  }

  @Before
  public void setUp() {
    rejectionReasonCategory = generateInstance();
    pageable = PageRequest.of(0, 10);
  }

  @Test
  public void shouldFindRejectionReasonCategoryByName() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = repository.save(rejectionReasonCategory);

    Set<RejectionReasonCategory> rejectionReasonCategories =
            repository.searchRejectionReasonCategory(REJECTION_REASON_CATEGORY_NAME,
                    null);
    assertEquals(1, rejectionReasonCategories.size());
    assertTrue(rejectionReasonCategories.stream().allMatch(result -> rejectionReasonCategories
            .contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldFindRejectionReasonCategoryByCode() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = repository.save(rejectionReasonCategory);

    Set<RejectionReasonCategory> rejectionReasonCategories = repository
            .searchRejectionReasonCategory(null,
                    REJECTION_REASON_CATEGORY_CODE);
    assertEquals(1, rejectionReasonCategories.size());
    assertTrue(rejectionReasonCategories.stream().allMatch(result -> rejectionReasonCategories
            .contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldFindRejectionReasonCategoryByNameAndCode() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = repository.save(rejectionReasonCategory);

    Set<RejectionReasonCategory> rejectionReasonCategories =
            repository.searchRejectionReasonCategory(REJECTION_REASON_CATEGORY_NAME,
                    REJECTION_REASON_CATEGORY_CODE);
    assertEquals(1, rejectionReasonCategories.size());
    assertTrue(rejectionReasonCategories.stream().allMatch(result -> rejectionReasonCategories
            .contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldFindActiveRejectionReasonCategory() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = repository.save(rejectionReasonCategory);

    Set<RejectionReasonCategory> rejectionReasonCategories = repository
            .findByActive(true);
    assertEquals(1, rejectionReasonCategories.size());
    assertTrue(rejectionReasonCategories.stream().allMatch(result -> rejectionReasonCategories
            .contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldReturnAllRejectionReasonCategoryIFallParameterAreNull() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = repository.save(rejectionReasonCategory);

    Set<RejectionReasonCategory> rejectionReasonCategories = repository
            .searchRejectionReasonCategory(null,
                    null);
    assertEquals(1, rejectionReasonCategories.size());
    assertTrue(rejectionReasonCategories.stream().allMatch(result -> rejectionReasonCategories
            .contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldReturnAllRejectionReasonCategory() {
    rejectionReasonCategory = generateRejectionReasonCategory();
    rejectionReasonCategory = repository.save(rejectionReasonCategory);

    Page rejectionReasons = repository.findAllWithoutSnapshots(pageable);

    assertEquals(1, rejectionReasons.getContent().size());
    assertEquals(1, rejectionReasons.getContent().size());
  }
}