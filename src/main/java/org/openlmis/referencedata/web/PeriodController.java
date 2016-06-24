package org.openlmis.referencedata.web;

import java.util.Set;
import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.validate.PeriodValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.rest.webmvc.PersistentEntityResource;
import org.springframework.data.rest.webmvc.PersistentEntityResourceAssembler;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.support.SessionStatus;

@RestController
@RepositoryRestController
public class PeriodController
{
    private Logger logger = LoggerFactory.getLogger(PeriodController.class);

    @Autowired @Qualifier("beforeSavePeriodValidator")
    PeriodValidator validator;

    @Autowired
    PeriodRepository periodRepository;

    @RequestMapping(path = "/api/periods/validate", method = RequestMethod.POST)
    public boolean getValid(@RequestBody Period period, BindingResult bindingResult, SessionStatus status)
    {
        validator.validate(period, bindingResult);
        return bindingResult.getErrorCount() == 0;
    }

    @RequestMapping(value = "/periods", method = RequestMethod.POST)
    public ResponseEntity<?> createPeriod(@RequestBody Period period) {
        if (period == null) {
            return new ResponseEntity(HttpStatus.BAD_REQUEST);
        } else {
            logger.debug("Creating new period");
            Period newPeriod = periodRepository.save(period);
            return new ResponseEntity<Period>(newPeriod, HttpStatus.CREATED);
        }
    }
}