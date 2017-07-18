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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;
import static org.springframework.data.domain.Sort.Direction.ASC;
import static org.springframework.data.domain.Sort.Direction.DESC;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

@RunWith(MockitoJUnitRunner.class)
public class BasicRequisitionDtoComparatorTest {

  @Mock
  private Pageable pageable;

  @Mock
  private ProgramDto program1;

  @Mock
  private ProgramDto program2;

  @Mock
  private BasicRequisitionDto requisition1;

  @Mock
  private BasicRequisitionDto requisition2;

  @Before
  public void setUp() throws Exception {
    when(requisition1.getProgram()).thenReturn(program1);
    when(requisition2.getProgram()).thenReturn(program2);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionIfComparatorDoesNotExist() throws Exception {
    when(pageable.getSort()).thenReturn(new Sort("abc"));

    BasicRequisitionDtoComparator comparator = new BasicRequisitionDtoComparator(pageable);
    comparator.compare(requisition1, requisition2);
  }

  @Test
  public void shouldCompareBySingleField() throws Exception {
    when(requisition1.getEmergency()).thenReturn(true);
    when(requisition2.getEmergency()).thenReturn(false);

    when(pageable.getSort()).thenReturn(new Sort(new Sort.Order(DESC, "emergency")));

    BasicRequisitionDtoComparator comparator = new BasicRequisitionDtoComparator(pageable);

    assertThat(comparator.compare(requisition1, requisition2), is(greaterThan(0)));
    assertThat(comparator.compare(requisition2, requisition1), is(lessThan(0)));
    assertThat(comparator.compare(requisition1, requisition1), is(equalTo(0)));
    assertThat(comparator.compare(requisition2, requisition2), is(equalTo(0)));
  }

  @Test
  public void shouldCompareBySeveralFields() throws Exception {
    when(requisition1.getEmergency()).thenReturn(true);
    when(requisition2.getEmergency()).thenReturn(true);

    when(program1.getName()).thenReturn("A");
    when(program2.getName()).thenReturn("D");

    when(pageable.getSort()).thenReturn(
        new Sort(new Sort.Order(DESC, "emergency"), new Sort.Order(ASC, "programName"))
    );


    BasicRequisitionDtoComparator comparator = new BasicRequisitionDtoComparator(pageable);

    assertThat(comparator.compare(requisition1, requisition2), is(greaterThan(0)));
    assertThat(comparator.compare(requisition2, requisition1), is(lessThan(0)));
    assertThat(comparator.compare(requisition1, requisition1), is(equalTo(0)));
    assertThat(comparator.compare(requisition2, requisition2), is(equalTo(0)));
  }
}