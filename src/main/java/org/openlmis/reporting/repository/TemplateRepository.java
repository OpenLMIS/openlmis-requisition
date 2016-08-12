package org.openlmis.reporting.repository;

import org.openlmis.reporting.model.Template;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface TemplateRepository extends PagingAndSortingRepository<Template, UUID> {

  @Override
  @RestResource(exported = false)
  void deleteAll();

  Template findByName(@Param("name") String name);
}
