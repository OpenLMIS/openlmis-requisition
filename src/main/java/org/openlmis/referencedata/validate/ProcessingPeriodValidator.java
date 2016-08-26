package org.openlmis.referencedata.validate;

import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;
import org.openlmis.referencedata.service.ProcessingPeriodService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

import java.time.LocalDate;

public class ProcessingPeriodValidator implements Validator {
  @Autowired
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ProcessingPeriodService periodService;

  @Override
  public boolean supports(Class<?> clazz) {
    return ProcessingPeriod.class.equals(clazz);
  }

  @Override
  public void validate(Object obj, Errors err) {
    ValidationUtils.rejectIfEmpty(err, "startDate", "startDate.empty", "Start date is null");
    ValidationUtils.rejectIfEmpty(err, "endDate", "endDate.empty", "End date is null");

    ProcessingPeriod period = (ProcessingPeriod) obj;
    Iterable<ProcessingPeriod> iterable = periodService
            .searchPeriods(period.getProcessingSchedule(), null);

    LocalDate startDate = period.getStartDate();
    LocalDate endDate = period.getEndDate();

    if (endDate.isAfter(startDate)) {
      if (iterable.iterator().hasNext()) {
        ProcessingPeriod periodFromRepo = periodRepository.findFirst1ByOrderByEndDateDesc();
        LocalDate lastEndDate = periodFromRepo.getEndDate();
        if (!startDate.equals(lastEndDate.plusDays(1))) {
          err.rejectValue("startDate", "{gap.between.lastEndDate.and.startDate.validation.error}",
                "Start date should be one day after last added end date");
        }
      }
    } else {
      err.rejectValue("startDate", "{startDate.after.endDate.validation.error}",
              "Start date should be before end date");
      err.rejectValue("endDate", "{startDate.after.endDate.validation.error}",
              "End date should be after start date");
    }
  }
}