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

package org.openlmis.requisition.code;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Keeps counters in the {@code code_sequences} table of the service schema. The upsert takes a
 * row lock that lasts until the surrounding transaction ends, so concurrent callers for the same
 * key are serialised and a rollback releases the value.
 */
@Repository
public class JpaCodeSequenceRepository implements CodeSequenceRepository {

  private static final String UPSERT_SQL =
      "INSERT INTO %s.code_sequences (scopekey, lastvalue) VALUES (?1, 1) "
          + "ON CONFLICT (scopekey) DO UPDATE "
          + "SET lastvalue = code_sequences.lastvalue + 1, updateddate = now() "
          + "RETURNING lastvalue";

  @PersistenceContext
  private EntityManager entityManager;

  private final String upsertSql;

  public JpaCodeSequenceRepository(
      @Value("${spring.jpa.properties.hibernate.default_schema}") String schema) {
    this.upsertSql = String.format(UPSERT_SQL, schema);
  }

  @Override
  @Transactional(propagation = Propagation.MANDATORY)
  public int next(String scopeKey) {
    Query query = entityManager.createNativeQuery(upsertSql);
    query.setParameter(1, scopeKey);
    return ((Number) query.getSingleResult()).intValue();
  }
}
