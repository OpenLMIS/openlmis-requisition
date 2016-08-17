package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.service.PeriodService;
import org.openlmis.referencedata.validate.PeriodValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@RepositoryRestController
public class PeriodController {

  private static final Logger LOGGER = LoggerFactory.getLogger(PeriodController.class);

  @Autowired @Qualifier("beforeSavePeriodValidator")
  private PeriodValidator validator;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ExposedMessageSource messageSource;

  @Autowired
  private PeriodService periodService;

  /**
   * Finds periods matching all of provided parameters.
   * @param processingSchedule processingSchedule of searched Periods.
   * @param toDate to which day shall Period start.
   * @return ResponseEntity with list of all Periods matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/periods/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchPeriods(
          @RequestParam(value = "processingSchedule", required = true) Schedule processingSchedule,
          @RequestParam(value = "toDate", required = false)
          @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate) {
    List<Period> result = periodService.searchPeriods(processingSchedule, toDate);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Creates given period if possible.
   *
   * @param period Period object to be created.
   * @param bindingResult Object used for validation.
   * @return ResponseEntity with created Period, BAD_REQUEST otherwise.
   */
  @RequestMapping(value = "/periods", method = RequestMethod.POST)
  public ResponseEntity<?> createPeriod(@RequestBody Period period,
                                        BindingResult bindingResult) {
    LOGGER.debug("Creating new period");
    validator.validate(period, bindingResult);
    if (bindingResult.getErrorCount() == 0) {
      Period newPeriod = periodRepository.save(period);
      return new ResponseEntity<Period>(newPeriod, HttpStatus.CREATED);
    } else {
      return new ResponseEntity(getPeriodErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }
  }

  private Map<String, String> getPeriodErrors(final BindingResult bindingResult) {
    return new HashMap<String, String>() {
      {
        for (FieldError error : bindingResult.getFieldErrors()) {
          put(error.getField(), error.getDefaultMessage());
        }
      }
    };
  }

  /**
   * Get all periods.
   *
   * @return Periods.
   */
  @RequestMapping(value = "/periods", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllPeriods() {
    Iterable<Period> periods = periodRepository.findAll();
    if (periods == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(periods, HttpStatus.OK);
    }
  }

  /**
   * Get choosen period.
   *
   * @param periodId UUID of period which we want to get
   * @return Period.
   */
  @RequestMapping(value = "/periods/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getPeriod(@PathVariable("id") UUID periodId) {
    Period period = periodRepository.findOne(periodId);
    if (period == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(period, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting period.
   *
   * @param periodId UUID of period which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/periods/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deletePeriod(@PathVariable("id") UUID periodId) {
    Period period = periodRepository.findOne(periodId);
    if (period == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        periodRepository.delete(period);
      } catch (DataIntegrityViolationException ex) {
        LOGGER.debug("Period cannot be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<Period>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Returns total difference between start date and end date from given Period.
   *
   * @param periodId UUID of given period.
   * @return String which contains information about this difference.
   */
  @RequestMapping(value = "/periods/{id}/difference", method = RequestMethod.GET)
  @ResponseBody
  public String getTotalDifference(@PathVariable("id") UUID periodId) {
    Period period = periodRepository.findOne(periodId);

    java.time.Period total = java.time.Period.between(period.getStartDate(), period.getEndDate());
    String months = Integer.toString(total.getMonths());
    String days = Integer.toString(total.getDays() + 1);

    String[] msgArgs = {months, days};
    LOGGER.debug("Returning total days and months of schedule periods");

    return messageSource.getMessage("requisition.message.totalPeriod", msgArgs, LocaleContextHolder
            .getLocale());
  }
}