package org.openlmis.referencedata.validate;

import com.google.common.collect.Lists;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

import java.time.LocalDate;

public class PeriodValidator implements Validator {
  @Autowired
  private PeriodRepository periodRepository;

  @Override
  public boolean supports(Class<?> clazz) {
    return Period.class.equals(clazz);
  }

  @Override
  public void validate(Object obj, Errors err) {
    ValidationUtils.rejectIfEmpty(err, "startDate", "startDate.empty", "Start date is null");
    ValidationUtils.rejectIfEmpty(err, "endDate", "endDate.empty", "End date is null");

    Period period = (Period) obj;
    Iterable<Period> iterable = periodRepository
            .findByProcessingSchedule(period.getProcessingSchedule());

    LocalDate startDate = period.getStartDate();
    LocalDate endDate = period.getEndDate();

    if (endDate.isAfter(startDate)) {
      if (iterable.iterator().hasNext()) {
        Period periodFromRepo = lastPeriod(iterable);
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

  private Period lastPeriod(Iterable<Period> iterable) {
    int size = Lists.newArrayList(iterable).size();
    Period last = Lists.newArrayList(iterable).get(size - 1);
    for (Period p : iterable) {
      if (p.getEndDate().isAfter(last.getEndDate())) {
        last = p;
      }
    }
    return last;
  }
}