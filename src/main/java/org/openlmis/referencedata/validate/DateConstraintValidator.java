package org.openlmis.referencedata.validate;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.springframework.beans.factory.annotation.Autowired;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Date;
import javax.validation.constraintvalidation.ValidationTarget;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

@SuppressWarnings("unused")
public class DateConstraintValidator implements ConstraintValidator<DateValidator, Period> {

    @Autowired
    PeriodRepository periodRepository;

    private String start;
    private String end;

    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MMM-dd");

    @Override
    public void initialize(DateValidator constraint) {
        this.start = constraint.start();
        this.end = constraint.end();
    }

    @Override
    public boolean isValid(Period object, ConstraintValidatorContext cxt) {
        Schedule schedule = object.getProcessingSchedule();
        Iterable<Period> iterable = periodRepository.findByProcessingSchedule(schedule);

        LocalDate startDate = object.getStartDate();
        LocalDate endDate = object.getEndDate();

        if (!endDate.isBefore(startDate)){
            if(countSizeOfIterable(iterable) != 0) {
                Period periodFromRepo = iterable.iterator().next();
                LocalDate lastEndDate = periodFromRepo.getEndDate();
                if (startDate.equals(lastEndDate.plusDays(1)) || startDate.equals(lastEndDate)) {
                    return true;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        return false;
    }

    private int countSizeOfIterable(Iterable<Period> iterable) {
        int size = 0;
        for(Period p : iterable){
            size++;
        }
        return size;
    }
}