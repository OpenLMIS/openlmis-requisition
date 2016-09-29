package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.custom.RequisitionTemplateRepositoryCustom;

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
   * @param program Program of searched requisition template.
   * @return RequisitionTemplate with matched parameters.
   */
  public RequisitionTemplate searchRequisitionTemplates(UUID program) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<RequisitionTemplate> query = builder.createQuery(RequisitionTemplate.class);
    Root<RequisitionTemplate> root = query.from(RequisitionTemplate.class);
    Predicate predicate = builder.conjunction();

    if (program != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("program"), program));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getSingleResult();
  }
}
