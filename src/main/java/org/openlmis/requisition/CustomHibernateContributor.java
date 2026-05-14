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

package org.openlmis.requisition;

import org.hibernate.boot.MetadataBuilder;
import org.hibernate.boot.spi.MetadataBuilderContributor;
import org.hibernate.dialect.function.SQLFunctionTemplate;
import org.hibernate.type.BooleanType;

// registers Hibernate functions for two array-driven predicates:
// text_any(col, arr)
// uuid_pair_any(p, s, parr, sarr)
public class CustomHibernateContributor implements MetadataBuilderContributor {

  @Override
  public void contribute(MetadataBuilder metadataBuilder) {
    metadataBuilder.applySqlFunction("text_any",
        new SQLFunctionTemplate(BooleanType.INSTANCE, "?1 = ANY(?2)"));

    metadataBuilder.applySqlFunction("uuid_pair_any",
        new SQLFunctionTemplate(BooleanType.INSTANCE,
            "EXISTS (SELECT 1 FROM unnest(?3::uuid[], ?4::uuid[]) AS t(p,s) "
                + "WHERE t.p = ?1 AND t.s = ?2)"));
  }
}
