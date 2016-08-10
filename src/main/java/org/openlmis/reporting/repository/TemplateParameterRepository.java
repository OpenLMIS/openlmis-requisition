package org.openlmis.reporting.repository;

import org.openlmis.reporting.model.TemplateParameter;
import org.springframework.data.repository.Repository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface TemplateParameterRepository extends Repository<TemplateParameter, UUID> {

  @RestResource(exported = false)
  void deleteAll();

  TemplateParameter save(TemplateParameter entity);

  Iterable<TemplateParameter> save(Iterable<TemplateParameter> entities);

  /**
   * Retrieves an entity by its id.
   *
   * @param id must not be {@literal null}.
   * @return the entity with the given id or {@literal null} if none found
   * @throws IllegalArgumentException if {@code id} is {@literal null}
   */
  @RestResource(exported = false)
  TemplateParameter findOne(UUID id);
}
