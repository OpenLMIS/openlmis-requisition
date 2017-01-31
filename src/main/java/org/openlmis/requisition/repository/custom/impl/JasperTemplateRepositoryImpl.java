package org.openlmis.requisition.repository.custom.impl;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import org.openlmis.requisition.repository.custom.JasperTemplateRepositoryCustom;

public class JasperTemplateRepositoryImpl implements JasperTemplateRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  /**
   * Executes the (arbitrary) SQL string passed in as a parameter and returns a list of string 
   * values. If no results, the list is an empty list.
   */
  public List<String> runArbitrarySql(String arbitrarySql) {

    Query query = entityManager.createNativeQuery(arbitrarySql);
    return (List<String>) query.getResultList();
  }
}
