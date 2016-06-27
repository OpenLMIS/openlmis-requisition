package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.springframework.data.repository.query.Param;

import java.util.ArrayList;
import java.util.UUID;

public interface RequisitionTemplateRepository extends ReferenceDataRepository<RequisitionTemplate, UUID> {
    // if the relation is OneToOne, simply RequisitionTemplate can be return value
    //ArrayList<RequisitionTemplate> findByProgram(@Param("programid") Program program);
    //Iterable<RequisitionTemplate> findByRemarks(@Param("remarks") String remark);
}
