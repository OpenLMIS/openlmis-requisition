package org.openlmis.requisition.repository;

import java.util.UUID;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.repository.custom.JasperTemplateRepositoryCustom;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

public interface JasperTemplateRepository extends PagingAndSortingRepository<JasperTemplate, UUID>,
    JasperTemplateRepositoryCustom {

  JasperTemplate findByName(@Param("name") String name);
}
