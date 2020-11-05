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
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.dto.RejectionReasonDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.repository.RejectionReasonRepository;
import org.openlmis.requisition.testutils.RejectionReasonDataBuilder;

@SuppressWarnings({"PMD.UnusedPrivateField", "PMD.TooManyMethods"})
public class RejectionReasonControllerTest {

  @Mock
  private RejectionReasonRepository repository;

  @InjectMocks
  private RejectionReasonController controller = new RejectionReasonController();

  private String rejectionReasonName1;
  private String rejectionReasonCode1;
  private RejectionReason rejectionReason1;
  private String rejectionReasonName2;
  private String rejectionReasonCode2;
  private RejectionReason rejectionReason2;
  private Set<RejectionReason> rejectionReasons;
  private RejectionReasonDto rejectionReasonDto1;
  private RejectionReasonDto rejectionReasonDto2;
  private RejectionReasonCategory rejectionReasonCategory;

  /**
   * Constructor for test.
   */
  public RejectionReasonControllerTest() {
    initMocks(this);
    rejectionReasonCategory = new RejectionReasonCategory();

    rejectionReasonCode1 = "rejectionReasonCode1";
    rejectionReasonName1 = "rejectionReason1";
    rejectionReason1 = new RejectionReasonDataBuilder()
            .withName(rejectionReasonName1).build();

    rejectionReasonName2 = "rejectionReason2";
    rejectionReasonCode2 = "rejectionReasonCode2";
    rejectionReason2 = new RejectionReasonDataBuilder()
            .withName(rejectionReasonName2).build();

    rejectionReason1 =
            new RejectionReasonDataBuilder().withName(rejectionReasonName1)
                    .withCategory(rejectionReasonCategory)
                    .withCode(rejectionReasonCode1).build();

    rejectionReasonDto1 = new RejectionReasonDto();
    rejectionReason1.export(rejectionReasonDto1);

    rejectionReason2 =
            new RejectionReasonDataBuilder().withName(rejectionReasonCode2)
                    .withCategory(rejectionReasonCategory)
                    .withCode(rejectionReasonCode2).build();
    rejectionReasons = Sets.newHashSet(rejectionReason1,rejectionReason2);

    rejectionReasonDto2 = new RejectionReasonDto();
    rejectionReason2.export(rejectionReasonDto2);
  }

  private void preparePostOrPut() {
    when(repository.findFirstByName(rejectionReasonName1))
            .thenReturn(rejectionReason1);
    when(repository.findFirstByName(rejectionReasonName2))
            .thenReturn(rejectionReason2);
  }

  @Test
  public void shouldGetAllRejectionReason() {
    //given
    Set<RejectionReasonDto> expectedRejectionReasonDtos = Sets.newHashSet(rejectionReasonDto1,
            rejectionReasonDto2);
    when(repository.findAll()).thenReturn(rejectionReasons);

    //when
    Set<RejectionReasonDto> rejectionReasonDto1s = controller.getAllRejectionReasons();

    //then
    assertEquals(expectedRejectionReasonDtos, rejectionReasonDto1s);
  }

  @Test
  public void shouldGetRejectionReason() {
    //given
    when(repository.findById(rejectionReason1.getId()))
            .thenReturn(Optional.of(rejectionReason1));

    //when
    RejectionReasonDto rejectionReasonDto1 =
            controller.getRejectionReason(rejectionReason1.getId());

    //then
    assertEquals(rejectionReasonDto1, rejectionReasonDto1);
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldNotGetNonExistingRejectionReason() {
    //given
    when(repository.findById(rejectionReason1.getId())).thenReturn(Optional.empty());

    //when
    controller.getRejectionReason(rejectionReason1.getId());
  }

  @Test
  public void shouldCreateNewRejectionReasonOnPost() {
    //given
    preparePostOrPut();

    when(repository.findFirstByName(rejectionReasonName1)).thenReturn(rejectionReason1);
    when(repository.save(any())).thenReturn(rejectionReason1);

    //when
    controller.saveRejectionReasons(rejectionReasonDto1);

    //then
    verify(repository).save(rejectionReason1);
  }

  @Test
  public void shouldNotCreateExistingRejectionReasonOnPost() {
    //given
    preparePostOrPut();
    when(repository.save(any())).thenReturn(rejectionReason1);

    //when
    controller.saveRejectionReasons(rejectionReasonDto1);
  }

  @Test
  public void shouldDeleteExistingRejectionReason() {
    //given
    when(repository.findById(rejectionReason1.getId()))
            .thenReturn(Optional.of(rejectionReason1));

    //when
    controller.deleteRejectionReason(rejectionReason1.getId());

    //then
    verify(repository).deleteById(rejectionReason1.getId());
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldNotDeleteNonExistingRejectionReason() {
    //given
    when(repository.findById(rejectionReason1.getId()))
            .thenReturn(Optional.empty());

    //when
    controller.deleteRejectionReason(rejectionReason1.getId());
  }
}
