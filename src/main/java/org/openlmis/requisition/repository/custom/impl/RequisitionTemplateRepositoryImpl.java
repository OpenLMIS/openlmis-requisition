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

package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.repository.custom.RequisitionTemplateRepositoryCustom;
import org.openlmis.requisition.utils.Message;

import java.util.List;
import java.util.UUID;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

public class RequisitionTemplateRepositoryImpl implements RequisitionTemplateRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns Requisition templates with matched parameters.
   *
   * @param program Program of searched requisition template.
   * @return RequisitionTemplate with matched parameters.
   */
  public RequisitionTemplate getTemplateForProgram(UUID program) {
    if (program == null) {
      throw new ContentNotFoundMessageException(new Message("requisition.error"
          + ".program-cannot-be-null"));
    }
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<RequisitionTemplate> query = builder.createQuery(RequisitionTemplate.class);
    Root<RequisitionTemplate> root = query.from(RequisitionTemplate.class);
    Predicate predicate = builder.conjunction();

    predicate = builder.and(
        predicate,
        builder.equal(
            root.get("programId"), program));

    query.where(predicate);
    query.orderBy(builder.desc(root.get("createdDate")));

    List<RequisitionTemplate> templates = entityManager.createQuery(query)
        .setMaxResults(1).getResultList();
    return templates.isEmpty() ? null : templates.get(0);
  }
}
