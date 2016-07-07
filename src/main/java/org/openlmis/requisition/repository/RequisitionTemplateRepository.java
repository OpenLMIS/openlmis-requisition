package org.openlmis.requisition.repository;

import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface RequisitionTemplateRepository
        extends ReferenceDataRepository<RequisitionTemplate, UUID> {
  @Override
  @RestResource
  <S extends RequisitionTemplate> S save(S entity);

  @Override
  @RestResource
  <S extends RequisitionTemplate> Iterable<S> save(Iterable<S> entities);
}
