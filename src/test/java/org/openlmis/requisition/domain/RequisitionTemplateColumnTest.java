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

package org.openlmis.requisition.domain;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class RequisitionTemplateColumnTest {
  private RequisitionTemplateColumn requisitionTemplateColumn;

  @Before
  public void setUp() {
    requisitionTemplateColumn = new RequisitionTemplateColumn(null);
    requisitionTemplateColumn.setName("Test Column");
  }

  @Test
  public void testShouldChangeLabelOnlyIfValid() {
    requisitionTemplateColumn.setLabel("ValidName");
    Assert.assertEquals("ValidName", requisitionTemplateColumn.getLabel());
    requisitionTemplateColumn.setLabel("New valid name with numbers 123 and spaces ");
    Assert.assertEquals("New valid name with numbers 123 and spaces ",
        requisitionTemplateColumn.getLabel());
  }

}
