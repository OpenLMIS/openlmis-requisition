package org.openlmis.utils;

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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
