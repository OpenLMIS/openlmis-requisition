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

package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import com.google.common.collect.Sets;

import java.util.Optional;
import java.util.Set;
import  org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.dto.RejectionReasonCategoryDto;
import org.openlmis.requisition.exception.NotFoundException;
import org.openlmis.requisition.repository.RejectionReasonCategoryRepository;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;

@SuppressWarnings({"PMD.UnusedPrivateField", "PMD.TooManyMethods"})
public class RejectionReasonCategoryControllerTest {

  @Mock
  private RejectionReasonCategoryRepository repository;

  @InjectMocks
  private RejectionReasonCategoryController controller = new RejectionReasonCategoryController();

  private String rejectionReasonCategoryName1;
  private String rejectionReasonCategoryCode1;
  private RejectionReasonCategory rejectionReasonCategory1;
  private String rejectionReasonCategoryName2;
  private String rejectionReasonCategoryCode2;
  private RejectionReasonCategory rejectionReasonCategory2;
  private Set<RejectionReasonCategory> rejectionReasonCategories;
  private RejectionReasonCategoryDto rejectionReasonCategoryDto1;
  private RejectionReasonCategoryDto rejectionReasonCategoryDto2;

  /**
   * Constructor for test.
   */
  public RejectionReasonCategoryControllerTest() {
    initMocks(this);

    rejectionReasonCategoryName1 = "rejectionReasonCategoryName1";
    rejectionReasonCategoryCode1 = "rejectionReasonCategoryCode1";
    rejectionReasonCategory1 =
            new RejectionReasonCategoryDataBuilder().withName(rejectionReasonCategoryName1)
                    .withCode(rejectionReasonCategoryCode1).build();

    rejectionReasonCategoryDto1 = new RejectionReasonCategoryDto();
    rejectionReasonCategory1.export(rejectionReasonCategoryDto1);

    rejectionReasonCategoryName2 = "rejectionReasonCategoryName2";
    rejectionReasonCategoryCode2 = "rejectionReasonCategoryCode2";
    rejectionReasonCategory2 =
            new RejectionReasonCategoryDataBuilder().withName(rejectionReasonCategoryName2)
                    .withCode(rejectionReasonCategoryCode2).build();

    rejectionReasonCategoryDto2 = new RejectionReasonCategoryDto();
    rejectionReasonCategory2.export(rejectionReasonCategoryDto2);

    rejectionReasonCategories = Sets.newHashSet(rejectionReasonCategory1, rejectionReasonCategory2);
  }

  private void preparePostOrPut() {
    when(repository.findFirstByName(rejectionReasonCategoryName1))
            .thenReturn(rejectionReasonCategory1);
  }

  @Test
  public void shouldGetAllRejectionReasonCategories() {
    //given
    Set<RejectionReasonCategoryDto> expectedRejectionReasonCategoryDtos =
            Sets.newHashSet(rejectionReasonCategoryDto1,rejectionReasonCategoryDto2);
    when(repository.findAll()).thenReturn(rejectionReasonCategories);
    //when
    Set<RejectionReasonCategoryDto> rejectionReasonCategoryDtos =
            controller.getAllRejectionReasonCategories();

    //then
    assertEquals(expectedRejectionReasonCategoryDtos, rejectionReasonCategoryDtos);
  }

  @Test
  public void shouldGetRejectionReason() {
    //given
    when(repository.findById(rejectionReasonCategory1.getId()))
            .thenReturn(Optional.of(rejectionReasonCategory1));

    //when
    RejectionReasonCategoryDto rejectionReasonCategoryDto =
            controller.getRejectionReasonCategory(rejectionReasonCategory1.getId());

    //then
    assertEquals(rejectionReasonCategoryDto, rejectionReasonCategoryDto);
  }

  @Test(expected = NotFoundException.class)
  public void shouldNotGetNonExistingRejectionReason() {
    //given
    when(repository.findById(rejectionReasonCategory1.getId())).thenReturn(Optional.empty());

    //when
    controller.getRejectionReasonCategory(rejectionReasonCategory1.getId());
  }

  @Test
  public void shouldNotCreateExistingRejectionReasonCategoryOnPost() {
    //given
    preparePostOrPut();
    when(repository.save(any())).thenReturn(rejectionReasonCategory1);

    //when
    controller.saveRejectionReasonCategory(rejectionReasonCategoryDto1);
  }

  @Test
  public void shouldDeleteExistingRejectionReason() {
    //given
    when(repository.findById(rejectionReasonCategory1.getId()))
            .thenReturn(Optional.of(rejectionReasonCategory1));

    //when
    controller.deleteRejectionReasonCategory(rejectionReasonCategory1.getId());

    //then
    verify(repository).deleteById(rejectionReasonCategory1.getId());
  }

  @Test(expected = NotFoundException.class)
  public void shouldNotDeleteNonExistingRejectionReason() {
    //given
    when(repository.findById(rejectionReasonCategory1.getId())).thenReturn(Optional.empty());

    //when
    controller.deleteRejectionReasonCategory(rejectionReasonCategory1.getId());
  }
}
