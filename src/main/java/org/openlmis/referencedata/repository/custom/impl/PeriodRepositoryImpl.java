package org.openlmis.referencedata.repository.custom.impl;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.custom.PeriodRepositoryCustom;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.util.List;

public class PeriodRepositoryImpl implements PeriodRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Finds Periods matching all of provided parameters.
   * @param processingSchedule processingSchedule of searched Periods.
   * @param toDate to which day shall Period start.
   * @return list of all Periods matching all of provided parameters.
   */
  public List<Period> searchPeriods(
          Schedule processingSchedule, LocalDate toDate) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Period> query = builder.createQuery(Period.class);
    Root<Period> root = query.from(Period.class);
    Predicate predicate = builder.conjunction();
    if (processingSchedule != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("processingSchedule"), processingSchedule));
    }
    if (toDate != null) {
      predicate = builder.and(
              predicate,
              builder.lessThanOrEqualTo(
                      root.get("startDate"), toDate));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
