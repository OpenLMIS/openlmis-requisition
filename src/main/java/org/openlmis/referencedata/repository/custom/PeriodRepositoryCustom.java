package org.openlmis.referencedata.repository.custom;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;

import java.time.LocalDate;
import java.util.List;

public interface PeriodRepositoryCustom {

  List<Period> searchPeriods(Schedule processingSchedule, LocalDate toDate);
}
