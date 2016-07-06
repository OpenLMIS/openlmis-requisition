package org.openlmis.requisition.repository;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface RequisitionTemplateRepository
        extends ReferenceDataRepository<RequisitionTemplate, UUID> {
  //Accessible via http://127.0.0.1:8080/api/requisitionTemplates/search/findByProgram?program=http://127.0.0.1:8080/api/programs/{programid}
  RequisitionTemplate findByProgram(@Param("program") Program programid);
}
