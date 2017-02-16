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

package org.openlmis.utils;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.web.RequisitionDtoBuilder;

import java.util.ArrayList;
import java.util.List;

@RunWith(MockitoJUnitRunner.class)
public class ConvertHelperTest {

  @Mock
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @InjectMocks
  private ConvertHelper convertHelper;

  @Test
  public void shouldconvertRequisitionListToRequisitionDtoList() throws Exception {
    //given
    when(requisitionDtoBuilder.build(any(Requisition.class))).thenReturn(new RequisitionDto());
    ArrayList<Requisition> requisitions = new ArrayList<>();
    int listSize = 5;
    for (int i = 0; i < listSize; i++) {
      requisitions.add(new Requisition());
    }

    //when
    List<RequisitionDto> requisitionDtos = convertHelper
        .convertRequisitionListToRequisitionDtoList(requisitions);

    //then
    for (int i = 0; i < listSize; i++) {
      verify(requisitionDtoBuilder).build(requisitions.get(i));
    }
    assertEquals(listSize, requisitionDtos.size());
  }
}
