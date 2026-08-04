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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class SourceTypeTest {

  @Test
  public void shouldCheckIfSourceIsReferenceType() {
    assertTrue(SourceType.REFERENCE_DATA.isReferenceSource());
    assertTrue(SourceType.STOCK_CARDS.isReferenceSource());
    assertFalse(SourceType.USER_INPUT.isReferenceSource());
    assertFalse(SourceType.CALCULATED.isReferenceSource());
  }

  @Test
  public void shouldCheckIfSourceIsStockType() {
    assertTrue(SourceType.STOCK_CARDS.isStockSource());
    assertFalse(SourceType.USER_INPUT.isStockSource());
    assertFalse(SourceType.CALCULATED.isStockSource());
    assertFalse(SourceType.REFERENCE_DATA.isStockSource());
    assertFalse(SourceType.PREVIOUS_REQUISITION.isStockSource());
  }

  @Test
  public void supplyingFacilityStockShouldBeNeitherReferenceNorStockSource() {
    // It is display-only and has no backing line-item property; treating it as a stock source would
    // make the line-item stock invariants reflect on a non-existent property and fail.
    assertFalse(SourceType.SUPPLYING_FACILITY_STOCK.isReferenceSource());
    assertFalse(SourceType.SUPPLYING_FACILITY_STOCK.isStockSource());
  }
}
