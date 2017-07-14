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
import static org.springframework.util.CollectionUtils.isEmpty;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import org.apache.commons.beanutils.BeanComparator;
import org.apache.commons.collections.comparators.ComparatorChain;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.exception.ValidationMessageException;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

public class BasicRequisitionDtoComparator implements Comparator<BasicRequisitionDto> {
  private static final Map<String, FieldComparator> AVAILABLE_COMPARATORS;

  static {
    AVAILABLE_COMPARATORS = Maps.newHashMap();
    AVAILABLE_COMPARATORS.put("emergency", new FieldComparator("emergency", false));
    AVAILABLE_COMPARATORS.put("programName",new FieldComparator("program.name"));
    AVAILABLE_COMPARATORS.put("facilityCode", new FieldComparator("facility.code"));
    AVAILABLE_COMPARATORS.put("facilityName", new FieldComparator("facility.name"));
  }

  private List<String> compareConditions;

  /**
   * Creates new instance with a single compare condition.
   */
  public BasicRequisitionDtoComparator(String compareCondition) {
    this(Collections.singletonList(compareCondition));
  }

  /**
   * Creates new instance with the passed compare conditions.
   */
  public BasicRequisitionDtoComparator(List<String> compareConditions) {
    this.compareConditions = isEmpty(compareConditions)
        ? Lists.newArrayList("emergency", "programName")
        : compareConditions;
  }

  @Override
  public int compare(BasicRequisitionDto o1, BasicRequisitionDto o2) {
    ComparatorChain chain = new ComparatorChain();

    for (int i = 0, size = compareConditions.size(); i < size; ++i) {
      String compareCondition = compareConditions.get(i);
      FieldComparator comparator = AVAILABLE_COMPARATORS.get(compareCondition);

      if (null == comparator) {
        throw new ValidationMessageException(
            new Message(ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING, compareCondition)
        );
      }

      chain.addComparator(comparator,  comparator.reverse);
    }

    return chain.compare(o1, o2);
  }


  private static final class FieldComparator extends BeanComparator {
    private boolean reverse;

    FieldComparator(String property) {
      this(property, true);
    }

    FieldComparator(String property, boolean reverse) {
      super(property);
      this.reverse = reverse;
    }

  }

}
