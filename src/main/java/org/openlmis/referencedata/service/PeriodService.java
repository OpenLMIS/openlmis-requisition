package org.openlmis.referencedata.service;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class PeriodService {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Finds Periods matching all of provided parameters.
   * @param processingSchedule processingSchedule of searched Periods.
   * @param startDate maximal start date of Periods.
   * @return list of all Periods matching all of provided parameters.
   */
  public List<Period> searchPeriods(
          Schedule processingSchedule, LocalDate startDate) {
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
    if (startDate != null) {
      predicate = builder.and(
              predicate,
              builder.lessThan(
                      root.get("startDate"), startDate));
      query.orderBy(builder.asc(root.get("startDate")));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

}
