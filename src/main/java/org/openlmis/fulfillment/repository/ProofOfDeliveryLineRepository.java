package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.springframework.data.repository.Repository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface ProofOfDeliveryLineRepository extends
    Repository<ProofOfDeliveryLine, UUID> {

  @RestResource(exported = false)
  void deleteAll();

  ProofOfDeliveryLine save(ProofOfDeliveryLine entity);

  Iterable<ProofOfDeliveryLine> save(Iterable<ProofOfDeliveryLine> entities);

  /**
   * Retrieves an entity by its id.
   *
   * @param id must not be {@literal null}.
   * @return the entity with the given id or {@literal null} if none found
   * @throws IllegalArgumentException if {@code id} is {@literal null}
   */
  @RestResource(exported = false)
  ProofOfDeliveryLine findOne(UUID id);
}
