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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.junit.After;
import org.junit.Test;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;


public class RejectionReasonCategoryRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RejectionReasonCategory> {

  @Autowired
  private RejectionReasonCategoryRepository repository;

  private RejectionReasonCategory rejectionReasonCategory;

  @Override
  RejectionReasonCategoryRepository getRepository() {
    return this.repository;
  }


  RejectionReasonCategory generateInstance() {
    return new RejectionReasonCategoryDataBuilder()
            .withCode("RRC2")
            .withName("name")
            .buildAsNew();
  }

  public void setUp() {
    rejectionReasonCategory = generateInstance();
    repository.save(rejectionReasonCategory);
  }

  @After
  public void cleanUp() {
    repository.deleteAll();
  }

  @Test
  public void shouldGetFirstByNameIfExists() {
    setUp();
    //given
    String nameToFind = rejectionReasonCategory.getName();

    //when
    RejectionReasonCategory foundRejectionReasonCategory = repository.findFirstByName(nameToFind);

    //then
    assertEquals(rejectionReasonCategory, foundRejectionReasonCategory);
  }

  @Test
  public void shouldFindRejectionReasonCategory() {
    setUp();
    //given
    String nameToFind = rejectionReasonCategory.getName();
    String code = rejectionReasonCategory.getCode();

    //when
    List<Set<RejectionReasonCategory>> rejectionReasonCategories = Arrays.asList(
            repository.searchRejectionReasonCategory(nameToFind, code),
            repository.searchRejectionReasonCategory(null, code),
            repository.searchRejectionReasonCategory(nameToFind, null)
    );

    //then
    assertEquals(3, rejectionReasonCategories.size());
    assertTrue(rejectionReasonCategories.stream()
            .allMatch(result -> result.contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldNotFindRejectionReasonCategoryIfIncorrectParametersAreProvided() {

    setUp();
    //given
    String actualName = rejectionReasonCategory.getName();
    String anotherName = "some other name";
    String actualCode = rejectionReasonCategory.getCode();
    String anotherCode = "fake code";

    //when
    List<Set<RejectionReasonCategory>> rejectionReasonResults = Arrays.asList(
            repository.searchRejectionReasonCategory(actualName, anotherCode),
            repository.searchRejectionReasonCategory(anotherName, anotherCode),
            repository.searchRejectionReasonCategory(anotherName, actualCode),
            repository.searchRejectionReasonCategory(anotherName, null),
            repository.searchRejectionReasonCategory(null, anotherCode)
    );

    //then
    assertEquals(5, rejectionReasonResults.size());
    assertTrue(rejectionReasonResults.stream()
            .noneMatch(result -> result.contains(rejectionReasonCategory)));
  }

  @Test
  public void shouldNotGetFirstByNameIfDoesNotExist() {
    setUp();
    //given
    String nameToFind = "does not exist";

    //when
    RejectionReasonCategory foundRejectionReasonCategory = repository.findFirstByName(nameToFind);

    //then
    assertNull(foundRejectionReasonCategory);
  }

}
