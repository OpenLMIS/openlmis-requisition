package org.openlmis.reporting.repository;

import org.openlmis.reporting.model.Template;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface TemplateRepository extends PagingAndSortingRepository<Template, UUID> {

  @Override
  void deleteAll();

  Template findByName(@Param("name") String name);
}
