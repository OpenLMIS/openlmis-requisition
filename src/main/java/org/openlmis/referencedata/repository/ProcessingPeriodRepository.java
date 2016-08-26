package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.repository.custom.ProcessingPeriodRepositoryCustom;

import java.util.UUID;

public interface ProcessingPeriodRepository extends
        ReferenceDataRepository<ProcessingPeriod, UUID>,
      ProcessingPeriodRepositoryCustom {

  ProcessingPeriod findFirst1ByOrderByEndDateDesc();
}
