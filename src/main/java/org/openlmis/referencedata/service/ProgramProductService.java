package org.openlmis.referencedata.service;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.springframework.stereotype.Service;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class ProgramProductService {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Finds ProgramProducts matching all of provided parameters.
   * @param program program of searched ProgramProducts.
   * @param fullSupply are the looking programProducts fullSupply.
   * @return list of all ProgramProducts matching all of provided parameters.
   */
  public List<ProgramProduct> searchProgramProducts(Program program, Boolean fullSupply) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProgramProduct> query = builder.createQuery(ProgramProduct.class);
    Root<ProgramProduct> root = query.from(ProgramProduct.class);
    Predicate predicate = builder.conjunction();

    if (program != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("program"), program));
    }
    if (fullSupply != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("fullSupply"), fullSupply));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
