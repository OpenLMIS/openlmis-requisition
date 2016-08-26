package org.openlmis.requisition.repository;

import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.custom.RequisitionTemplateRepositoryCustom;

import java.util.UUID;

public interface RequisitionTemplateRepository extends
        ReferenceDataRepository<RequisitionTemplate, UUID>,
        RequisitionTemplateRepositoryCustom {
  @Override
  <S extends RequisitionTemplate> S save(S entity);

  @Override
  <S extends RequisitionTemplate> Iterable<S> save(Iterable<S> entities);
}
