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
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
import org.openlmis.requisition.testutils.RejectionReasonDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;


public class RejectionReasonRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<RejectionReason> {

  @Autowired
  private RejectionReasonRepository repository;

  @Autowired
  private RejectionReasonCategoryRepository rejectionReasonCategoryRepository;

  private RejectionReasonCategory rejectionReasonCategory;

  private RejectionReason rejectionReason;

  @After
  public void cleanUp() {
    repository.deleteAll();
  }

  @Override
  RejectionReasonRepository getRepository() {
    return this.repository;
  }

  @Override
  RejectionReason generateInstance() {
    rejectionReasonCategory = rejectionReasonCategoryRepository
            .save(new RejectionReasonCategoryDataBuilder()
                    .withCode("RRC2")
                    .withName("name")
                    .buildAsNew());

    rejectionReasonCategoryRepository.save(rejectionReasonCategory);

    return new RejectionReasonDataBuilder()
            .withCode("RR1")
            .withName("name")
            .withCategory(rejectionReasonCategory)
            .buildAsNew();
  }


  public void setUp() {
    rejectionReason = this.generateInstance();
    repository.save(rejectionReason);
  }

  @Test
  public void shouldGetFirstByNameIfExists() {
    setUp();
    //given
    String nameToFind = rejectionReason.getName();

    //when
    RejectionReason foundRejectionReason = repository.findFirstByName(nameToFind);

    //then
    assertEquals(rejectionReason, foundRejectionReason);
  }

  @Test
  public void shouldFindRejectionReason() {
    setUp();
    //given
    String nameToFind = rejectionReason.getName();
    String code = rejectionReason.getCode();

    //when
    List<Set<RejectionReason>> rejectionReasonResults = Arrays.asList(
            repository.searchRejectionReason(nameToFind, code),
            repository.searchRejectionReason(null, code),
            repository.searchRejectionReason(nameToFind, null)
    );

    //then
    assertEquals(3, rejectionReasonResults.size());
    assertTrue(rejectionReasonResults.stream()
            .allMatch(result -> result.contains(rejectionReason)));
  }

  @Test
  public void shouldNotFindRejectionReasonIfIncorrectParametersAreProvided() {
    setUp();
    //given
    String actualName = rejectionReason.getName();
    String anotherName = "some other name";
    String actualCode = rejectionReason.getCode();
    String anotherCode = "fake code";

    //when
    List<Set<RejectionReason>> rejectionReasonResults = Arrays.asList(
            repository.searchRejectionReason(actualName, anotherCode),
            repository.searchRejectionReason(anotherName, anotherCode),
            repository.searchRejectionReason(anotherName, actualCode),
            repository.searchRejectionReason(anotherName, null),
            repository.searchRejectionReason(null, anotherCode)
    );

    //then
    assertEquals(5, rejectionReasonResults.size());
    assertTrue(rejectionReasonResults.stream()
            .noneMatch(result -> result.contains(rejectionReason)));
  }

  @Test
  public void shouldNotGetFirstByNameIfDoesNotExist() {
    setUp();
    //given
    String nameToFind = "does not exist";

    //when
    RejectionReason foundRejectionReason = repository.findFirstByName(nameToFind);

    //then
    assertNull(foundRejectionReason);
  }


}
