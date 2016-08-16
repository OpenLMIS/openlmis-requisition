package org.openlmis.referencedata.service;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Service
public class PeriodService {

  @Autowired
  private PeriodRepository periodRepository;

  /**
   * Finds Periods matching all of provided parameters.
   * @param processingSchedule processingSchedule of searched Periods.
   * @param toDate to which day shall Period start.
   * @return list of all Periods matching all of provided parameters.
   */
  public List<Period> searchPeriods(
          Schedule processingSchedule, LocalDate toDate) {
    return periodRepository.searchPeriods(processingSchedule, toDate);
  }

}
