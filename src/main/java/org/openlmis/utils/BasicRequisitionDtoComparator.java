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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING;
import static org.springframework.data.domain.Sort.Direction.ASC;
import static org.springframework.data.domain.Sort.Direction.DESC;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import org.apache.commons.beanutils.BeanComparator;
import org.apache.commons.collections.comparators.ComparatorChain;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import java.util.Comparator;
import java.util.List;
import java.util.Map;

public class BasicRequisitionDtoComparator implements Comparator<BasicRequisitionDto> {
  private static final Map<String, BeanComparator> AVAILABLE_COMPARATORS;

  static {
    AVAILABLE_COMPARATORS = Maps.newHashMap();
    AVAILABLE_COMPARATORS.put("emergency", new BeanComparator("emergency"));
    AVAILABLE_COMPARATORS.put("programName", new BeanComparator("program.name"));
    AVAILABLE_COMPARATORS.put("facilityCode", new BeanComparator("facility.code"));
    AVAILABLE_COMPARATORS.put("facilityName", new BeanComparator("facility.name"));
  }

  private List<Sort.Order> compareConditions;

  /**
   * Creates new instance with the passed pageable instance. If pageable instance does not contain
   * sort property (it is equal to null), the class will use default sort: emergency DESC and
   * programName ASC.
   */
  public BasicRequisitionDtoComparator(Pageable pageable) {
    Sort sort = pageable.getSort();

    if (null == sort) {
      sort = new Sort(
          new Sort.Order(DESC, "emergency"), new Sort.Order(ASC, "programName")
      );
    }

    compareConditions = Lists.newArrayList(sort);
  }

  @Override
  public int compare(BasicRequisitionDto o1, BasicRequisitionDto o2) {
    ComparatorChain chain = new ComparatorChain();

    for (int i = 0, size = compareConditions.size(); i < size; ++i) {
      Sort.Order order = compareConditions.get(i);
      String property = order.getProperty();
      BeanComparator comparator = AVAILABLE_COMPARATORS.get(property);

      if (null == comparator) {
        throw new ValidationMessageException(
            new Message(ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING, property)
        );
      }

      chain.addComparator(comparator, order.isAscending());
    }

    return chain.compare(o1, o2);
  }

}
