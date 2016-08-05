package org.openlmis.hierarchyandsupervision.service;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.Facility;
import org.springframework.stereotype.Service;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;


@Service
public class UserService {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns all users with matched parameters.
   * @param username username of user.
   * @param firstName firstName of user.
   * @param lastName lastName of user.
   * @param homeFacility homeFacility of user.
   * @param active is the account activated.
   * @param verified is the account verified.
   * @return List of users
   */
  public List<User> searchUsers(
          String username, String firstName, String lastName,
          Facility homeFacility, Boolean active, Boolean verified ) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<User> query = builder.createQuery(User.class);
    Root<User> root = query.from(User.class);
    Predicate predicate = builder.conjunction();
    if (username != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("username"), username));
    }
    if (firstName != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("firstName"), firstName));
    }
    if (lastName != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("lastName"), lastName));
    }
    if (homeFacility != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("homeFacility"), homeFacility));
    }
    if (active != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("active"), active));
    }
    if (verified != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("verified"), verified));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
