package org.openlmis.utils;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.RequisitionDto;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@RunWith(MockitoJUnitRunner.class)
public class PaginationHelperTest {

  private PaginationHelper paginationHelper = new PaginationHelper();

  private List<RequisitionDto> requisitionDtoList = new ArrayList<>();

  @Before
  public void setUp() {
    for (int i = 0; i < 5; i++) {
      RequisitionDto requisitionDto = new RequisitionDto();
      requisitionDto.setId(UUID.randomUUID());
      requisitionDtoList.add(requisitionDto);
    }
  }

  @Test
  public void shouldReturnEmptyListWhenFirstIndexGreaterOrEqualThanRequisitionListSize()
      throws Exception {
    List<RequisitionDto> requisitionDtoListRetrieved =
        paginationHelper.pageCollection(requisitionDtoList, 2, 5);

    assertTrue(requisitionDtoListRetrieved.isEmpty());
  }

  @Test
  public void shouldReturnAppropriateSubListFromRequisitionDtoList() throws Exception {
    List<RequisitionDto> requisitionDtoListRetrieved =
        paginationHelper.pageCollection(requisitionDtoList, 2, 2);

    assertEquals(2, requisitionDtoListRetrieved.size());
    assertEquals(requisitionDtoList.get(2).getId(), requisitionDtoListRetrieved.get(0).getId());
    assertEquals(requisitionDtoList.get(3).getId(), requisitionDtoListRetrieved.get(1).getId());
  }

  @Test
  public void shouldReturnAppropriateSubListForLastPage() throws
      Exception {
    List<RequisitionDto> requisitionDtoListRetrieved =
        paginationHelper.pageCollection(requisitionDtoList, 2, 4);

    assertEquals(1, requisitionDtoListRetrieved.size());
    assertEquals(requisitionDtoList.get(4).getId(), requisitionDtoListRetrieved.get(0).getId());
  }
}
