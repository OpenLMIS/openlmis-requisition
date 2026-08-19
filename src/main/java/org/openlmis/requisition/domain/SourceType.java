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

public enum SourceType {
  // Persisted by ORDINAL in columns_maps.source (RequisitionTemplateColumn.source has no
  // @Enumerated), so existing constants must keep their position: only append new values,
  // never reorder or insert.
  USER_INPUT,
  CALCULATED,
  REFERENCE_DATA,
  STOCK_CARDS,
  PREVIOUS_REQUISITION,
  // Ordinal 5, written as source = 5 by the requisition-template migration. It is deliberately
  // neither a reference nor a stock source: it has no backing line-item property, so treating it
  // as a stock source would make the line-item stock invariants reflect on a missing property.
  SUPPLYING_FACILITY_STOCK;

  public boolean isReferenceSource() {
    return REFERENCE_DATA.equals(this) || STOCK_CARDS.equals(this);
  }

  public boolean isStockSource() {
    return STOCK_CARDS.equals(this);
  }
}
