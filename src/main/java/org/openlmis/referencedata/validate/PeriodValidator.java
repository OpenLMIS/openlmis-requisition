package org.openlmis.referencedata.validate;

import java.time.LocalDate;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@SuppressWarnings("PMD.UnusedLocalVariable")
public class PeriodValidator implements Validator
{
    @Autowired
    PeriodRepository periodRepository;

    @Override
    public boolean supports(Class<?> clazz)
    {
        return Period.class.equals(clazz);
    }

    @Override
    public void validate(Object obj, Errors e)
    {
        ValidationUtils.rejectIfEmpty(e, "startDate", "startDate.empty", "Start date is null");
        ValidationUtils.rejectIfEmpty(e, "endDate", "endDate.empty", "End date is null");

        Period period = (Period) obj;
        Iterable<Period> iterable = periodRepository.findByProcessingSchedule(period.getProcessingSchedule());

        LocalDate startDate = period.getStartDate();
        LocalDate endDate = period.getEndDate();

        if (endDate.isAfter(startDate)){
            if(countSizeOfIterable(iterable) != 0) {
                Period periodFromRepo = iterable.iterator().next();
                LocalDate lastEndDate = periodFromRepo.getEndDate();
                if (!startDate.equals(lastEndDate.plusDays(1))) {
                    e.rejectValue("startDate", "{gap.between.lastEndDate.and.startDate.validation.error}",
                            "Start date should be one day after last added end date");
                }
            }
        }
        else {
            e.rejectValue("startDate", "{startDate.after.endDate.validation.error}",
                    "Start date should be before end date");
            e.rejectValue("endDate", "{startDate.after.endDate.validation.error}",
                    "End date should be after start date");
        }
    }

    private int countSizeOfIterable(Iterable<Period> iterable) {
        int size = 0;
        for(Period p : iterable){
            size++;
        }
        return size;
    }
}