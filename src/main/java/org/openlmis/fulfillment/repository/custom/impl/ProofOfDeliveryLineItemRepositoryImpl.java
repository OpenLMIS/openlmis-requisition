package org.openlmis.fulfillment.repository.custom.impl;

import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLineItem;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

public class ProofOfDeliveryLineItemRepositoryImpl {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method deletes given proof of delivery line.
   * @param entity entity to be deleted.
   */
  public void delete(ProofOfDeliveryLineItem entity) {
    ProofOfDelivery pod = entity.getProofOfDelivery();
    pod.getProofOfDeliveryLineItems().remove(entity);
    entityManager.merge(pod);
  }
}
