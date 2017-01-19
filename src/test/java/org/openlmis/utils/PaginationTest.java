package org.openlmis.utils;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@RunWith(MockitoJUnitRunner.class)
public class PaginationTest {

  @Test
  public void getPageReturnsTheCorrectPage() {
    int page = 1;
    int size = 3;
    PageRequest pageRequest = new PageRequest(page, size);

    Page<Integer> pagedList = Pagination.getPage(getList(), pageRequest);

    List<Integer> pagedListContent = pagedList.getContent();

    assertThat(pagedListContent.size()).isEqualTo(3);

    assertThat(pagedListContent.get(0)).isEqualTo(3);
    assertThat(pagedListContent.get(1)).isEqualTo(4);
    assertThat(pagedListContent.get(2)).isEqualTo(5);
  }


  @Test
  public void getPageReturnsEmptyResultIfSpecifiedPageNumberIsOutOfBounds() {
    int page = Integer.MAX_VALUE;
    int size = 5;
    PageRequest pageRequest = new PageRequest(page, size);

    Page<Integer> pagedList = Pagination.getPage(getList(), pageRequest);

    List<Integer> pagedListContent = pagedList.getContent();
    assertThat(pagedListContent.size()).isEqualTo(0);
  }


  @Test
  public void getPageReturnsAllValuesEvenWhenSizeIsOutOfBounds() {
    int page = 0;
    int size = Integer.MAX_VALUE;
    PageRequest pageRequest = new PageRequest(page, size);

    Page<Integer> pagedList = Pagination.getPage(getList(), pageRequest);

    List<Integer> pagedListContent = pagedList.getContent();
    assertThat(pagedListContent.size()).isEqualTo(getList().size());
  }

  @Test
  public void getPageReturnsSomeValuesEvenWhenSizeIsOutOfBounds() {
    int page = 1;
    int size = 7;
    PageRequest pageRequest = new PageRequest(page, size);

    Page<Integer> pagedList = Pagination.getPage(getList(), pageRequest);

    List<Integer> pagedListContent = pagedList.getContent();

    assertThat(pagedListContent.size()).isEqualTo(3);

    assertThat(pagedListContent.get(0)).isEqualTo(7);
    assertThat(pagedListContent.get(1)).isEqualTo(8);
    assertThat(pagedListContent.get(2)).isEqualTo(9);
  }

  private List<Integer> getList() {
    List<Integer> values = new ArrayList<Integer>() {{
        add(new Integer(0));
        add(new Integer(1));
        add(new Integer(2));
        add(new Integer(3));
        add(new Integer(4));
        add(new Integer(5));
        add(new Integer(6));
        add(new Integer(7));
        add(new Integer(8));
        add(new Integer(9));
      }
    };
    return values;
  }

}
