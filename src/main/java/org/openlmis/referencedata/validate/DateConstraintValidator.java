package org.openlmis.referencedata.validate;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Date;
import javax.validation.constraintvalidation.ValidationTarget;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.springframework.beans.factory.annotation.Autowired;

@SuppressWarnings("unused")
public class DateConstraintValidator implements ConstraintValidator<DateValidator, Period> {

    @Autowired
    PeriodRepository periodRepository;

    private DateValidator dateValidator;

    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MMM-dd");

    @Override
    public void initialize(DateValidator dateValidator) {
        this.dateValidator = dateValidator;
    }

    @Override
    public boolean isValid(Period object, ConstraintValidatorContext cxt) {

        if(object == null){
            return true;
        }

        boolean isValid = true;

        Schedule schedule = object.getProcessingSchedule();
        Iterable<Period> iterable = periodRepository.findByProcessingSchedule(schedule);

        LocalDate startDate = object.getStartDate();
        LocalDate endDate = object.getEndDate();

        if (endDate.isAfter(startDate)){
            if(countSizeOfIterable(iterable) != 0) {
                Period periodFromRepo = iterable.iterator().next();
                LocalDate lastEndDate = periodFromRepo.getEndDate();
                if (!startDate.equals(lastEndDate.plusDays(1))) {
                    cxt.disableDefaultConstraintViolation();
                    cxt.buildConstraintViolationWithTemplate("{gap.between.lastEndDate.and.startDate.validation.error}")
                            .addConstraintViolation();
                    isValid = false;
                }
            }
        }
        else {
            cxt.disableDefaultConstraintViolation();
            cxt.buildConstraintViolationWithTemplate("{startDate.after.endDate.validation.error}")
                    .addConstraintViolation();
            isValid = false;
        }
        return isValid;
    }

    private int countSizeOfIterable(Iterable<Period> iterable) {
        int size = 0;
        for(Period p : iterable){
            size++;
        }
        return size;
    }
}