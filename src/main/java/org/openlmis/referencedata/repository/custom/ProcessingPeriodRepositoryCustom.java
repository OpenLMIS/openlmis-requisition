package org.openlmis.referencedata.repository.custom;

import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.ProcessingSchedule;

import java.time.LocalDate;
import java.util.List;

public interface ProcessingPeriodRepositoryCustom {

  List<ProcessingPeriod> searchPeriods(ProcessingSchedule processingSchedule, LocalDate toDate);
}
