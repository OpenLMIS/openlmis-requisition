package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.JasperTemplate;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface JasperTemplateRepository extends PagingAndSortingRepository<JasperTemplate, UUID> {
  JasperTemplate findByName(@Param("name") String name);
}
