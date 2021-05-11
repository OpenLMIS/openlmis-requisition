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

package org.openlmis.requisition.domain.requisition;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.openlmis.requisition.service.RequisitionService;
import org.springframework.beans.factory.annotation.Autowired;


@AllArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@ToString
public class RequisitionUnSkippedDetails {

  @Autowired
  private RequisitionService requisitionService;

  private List<RequisitionUnSkippedLineItem> unSkippedLineItemList;

  private String username;
  private String firstname;
  private String lastname;

  /**
   * default constructor.
   */
  public RequisitionUnSkippedDetails() {
    unSkippedLineItemList = new ArrayList<>();
  }

  /**
   * adds items to unskippedlineitemsList.
   * @param name product name
   * @param code product code
   * @param quant approved quantity
   * @param remarks approver remarks
   */
  public void addUnSkippedLineItem(String name, String code,
                                   int quant, String remarks) {
    RequisitionUnSkippedLineItem lineItem =
              new RequisitionUnSkippedLineItem(code,name,quant,remarks);
    unSkippedLineItemList.add(lineItem);
  }

  @AllArgsConstructor
  @NoArgsConstructor
  @Getter
  @Setter
  class RequisitionUnSkippedLineItem {
    private String productCode;
    private String productName;
    private int approvedQuantity;
    private String remarks;
  }
}
