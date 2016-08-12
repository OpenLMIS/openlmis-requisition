package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.repository.custom.PeriodRepositoryCustom;

import java.util.UUID;

public interface PeriodRepository extends
        ReferenceDataRepository<Period, UUID>,
        PeriodRepositoryCustom {

  Period findFirst1ByOrderByEndDateDesc();
}
