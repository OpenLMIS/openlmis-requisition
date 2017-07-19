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
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.Requisition.EMERGENCY;
import static org.springframework.data.domain.Sort.Direction.ASC;
import static org.springframework.data.domain.Sort.Direction.DESC;

import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.BasicProgramDto;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import java.util.List;

@RunWith(MockitoJUnitRunner.class)
public class BasicRequisitionDtoComparatorTest {
  private static final String CODE = "code";
  private static final String NAME = "name";

  @Mock
  private Pageable pageable;

  private List<BasicRequisitionDto> requisitions;

  @Before
  public void setUp() throws Exception {
    requisitions = Lists.newArrayList(
        create(false, "A", "F1"), create(true, "C", "F2"),
        create(true, "A", "F6"), create(false, "B", "F3"),
        create(false, "C", "F5"), create(true, "B", "F4")
    );
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionIfComparatorDoesNotExist() throws Exception {
    when(pageable.getSort()).thenReturn(new Sort("abc"));

    BasicRequisitionDtoComparator comparator = new BasicRequisitionDtoComparator(pageable);
    comparator.compare(null, null);
  }

  @Test
  public void shouldCompareBySingleField() {
    // given
    Sort sort = new Sort(new Sort.Order(DESC, EMERGENCY));

    // when
    sortBy(sort);

    // then
    assertThat(requisitions.get(0), hasProperty(EMERGENCY, equalTo(true)));

    assertThat(requisitions.get(1), hasProperty(EMERGENCY, equalTo(true)));

    assertThat(requisitions.get(2), hasProperty(EMERGENCY, equalTo(true)));

    assertThat(requisitions.get(3), hasProperty(EMERGENCY, equalTo(false)));

    assertThat(requisitions.get(4), hasProperty(EMERGENCY, equalTo(false)));

    assertThat(requisitions.get(5), hasProperty(EMERGENCY, equalTo(false)));
  }

  @Test
  public void shouldCompareBySeveralFields() {
    // given
    Sort sort = new Sort(new Sort.Order(DESC, "emergency"), new Sort.Order(ASC, "facilityCode"));

    // when
    sortBy(sort);

    assertThat(requisitions.get(0), hasProperty(EMERGENCY, equalTo(true)));
    assertThat(requisitions.get(0).getFacility(), hasProperty(CODE, equalTo("F2")));

    assertThat(requisitions.get(1), hasProperty(EMERGENCY, equalTo(true)));
    assertThat(requisitions.get(1).getFacility(), hasProperty(CODE, equalTo("F4")));

    assertThat(requisitions.get(2), hasProperty(EMERGENCY, equalTo(true)));
    assertThat(requisitions.get(2).getFacility(), hasProperty(CODE, equalTo("F6")));

    assertThat(requisitions.get(3), hasProperty(EMERGENCY, equalTo(false)));
    assertThat(requisitions.get(3).getFacility(), hasProperty(CODE, equalTo("F1")));

    assertThat(requisitions.get(4), hasProperty(EMERGENCY, equalTo(false)));
    assertThat(requisitions.get(4).getFacility(), hasProperty(CODE, equalTo("F3")));

    assertThat(requisitions.get(5), hasProperty(EMERGENCY, equalTo(false)));
    assertThat(requisitions.get(5).getFacility(), hasProperty(CODE, equalTo("F5")));
  }

  @Test
  public void shouldCompareByDefaultSettings() {
    // when
    sortBy(null);

    assertThat(requisitions.get(0), hasProperty(EMERGENCY, equalTo(true)));
    assertThat(requisitions.get(0).getProgram(), hasProperty(NAME, equalTo("A")));

    assertThat(requisitions.get(1), hasProperty(EMERGENCY, equalTo(true)));
    assertThat(requisitions.get(1).getProgram(), hasProperty(NAME, equalTo("B")));

    assertThat(requisitions.get(2), hasProperty(EMERGENCY, equalTo(true)));
    assertThat(requisitions.get(2).getProgram(), hasProperty(NAME, equalTo("C")));

    assertThat(requisitions.get(3), hasProperty(EMERGENCY, equalTo(false)));
    assertThat(requisitions.get(3).getProgram(), hasProperty(NAME, equalTo("A")));

    assertThat(requisitions.get(4), hasProperty(EMERGENCY, equalTo(false)));
    assertThat(requisitions.get(4).getProgram(), hasProperty(NAME, equalTo("B")));

    assertThat(requisitions.get(5), hasProperty(EMERGENCY, equalTo(false)));
    assertThat(requisitions.get(5).getProgram(), hasProperty(NAME, equalTo("C")));
  }

  private BasicRequisitionDto create(boolean emergency, String programName, String facilityCode) {
    MinimalFacilityDto facility = new MinimalFacilityDto();
    facility.setCode(facilityCode);

    BasicProgramDto program = new BasicProgramDto();
    program.setName(programName);

    BasicRequisitionDto requisition = new BasicRequisitionDto();
    requisition.setEmergency(emergency);
    requisition.setProgram(program);
    requisition.setFacility(facility);

    return requisition;
  }

  private void sortBy(Sort sort) {
    when(pageable.getSort()).thenReturn(sort);
    requisitions.sort(new BasicRequisitionDtoComparator(pageable));
  }

}