package org.openlmis.referencedata.web;

import java.util.HashMap;
import java.util.Map;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.validate.PeriodValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.support.SessionStatus;

@RepositoryRestController
public class PeriodController
{
    private Logger logger = LoggerFactory.getLogger(PeriodController.class);

    @Autowired @Qualifier("beforeSavePeriodValidator")
    PeriodValidator validator;

    @Autowired
    PeriodRepository periodRepository;

    @RequestMapping(value = "/periods", method = RequestMethod.POST)
    public ResponseEntity<?> createPeriod(@RequestBody Period period, BindingResult bindingResult, SessionStatus status) {
        if (period == null) {
            return new ResponseEntity(HttpStatus.BAD_REQUEST);
        } else {
            logger.debug("Creating new period");
            period.setId(null);
            validator.validate(period, bindingResult);
            if(bindingResult.getErrorCount() == 0) {
                Period newPeriod = periodRepository.save(period);
                return new ResponseEntity<Period>(newPeriod, HttpStatus.CREATED);
            }
            else {
                return new ResponseEntity(getPeriodErrors(bindingResult), HttpStatus.BAD_REQUEST);
            }
        }
    }

    private Map<String, String> getPeriodErrors(final BindingResult bindingResult) {
        return new HashMap<String, String>() {{
            for (FieldError error : bindingResult.getFieldErrors()) {
                put(error.getField(), error.getDefaultMessage());
            }
        }};
    }
}