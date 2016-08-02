package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Period;

import java.util.UUID;

public interface PeriodRepository extends ReferenceDataRepository<Period, UUID> {

  Period findFirst1ByOrderByEndDateDesc();
}
