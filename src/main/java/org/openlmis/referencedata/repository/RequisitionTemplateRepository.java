package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.springframework.data.repository.query.Param;

import java.util.ArrayList;
import java.util.UUID;

public interface RequisitionTemplateRepository extends ReferenceDataRepository<RequisitionTemplate, UUID> {
    //Add custom Program related members here. See UserRepository.java for examples.
}
