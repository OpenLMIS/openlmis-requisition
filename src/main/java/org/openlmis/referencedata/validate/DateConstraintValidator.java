package org.openlmis.referencedata.validate;

import org.openlmis.referencedata.repository.PeriodRepository;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;

public class DateConstraintValidator implements ConstraintValidator<DateValidator, Period> {

    @Autowired
    PeriodRepository periodRepository;

    PeriodRepository getRepository() {
        return this.periodRepository;
    }

    private String start;
    private String end;

    @Override
    public void initialize(DateValidator constraint) {
        start = constraint.start();
        end = constraint.end();
    }

    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MMM-dd");

    @Override
    public boolean isValid(Period period, ConstraintValidatorContext cxt) {
        Iterable<Period> iterable = periodRepository.findAll();

        if(countSizeOfIterable(iterable) != 0){
            Period periodFromRepo = iterable.iterator().next();
            if(!periodFromRepo.equals(null)){
                LocalDate lastEndDate = periodFromRepo.getEndDate();
                LocalDate startDate = LocalDate.parse(start, formatter);
                long days = ChronoUnit.DAYS.between(startDate, lastEndDate);
                return days == 1;
            }
        }
        return true;
    }

    private int countSizeOfIterable(Iterable<Period> iterable) {
        int size = 0;
        for(Period p : iterable) size++;
        return size;
    }
}