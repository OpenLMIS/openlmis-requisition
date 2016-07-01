package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.exception.EmptyObjectException;
import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RepositoryRestController
public class ScheduleController {
  private Logger logger = LoggerFactory.getLogger(ScheduleController.class);

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ExposedMessageSource messageSource;

  @RequestMapping(value = "/schedules", method = RequestMethod.POST)
  public ResponseEntity<?> createSchedule(@RequestBody Schedule schedule) throws EmptyObjectException {
    if (schedule == null) {
      throw new EmptyObjectException("Schedule's fields cannot be empty");
    } else {
      logger.debug("Creating new schedule");
      Schedule newSchedule = scheduleRepository.save(schedule);
      return new ResponseEntity<Schedule>(newSchedule, HttpStatus.CREATED);
    }
  }

  @RequestMapping(value = "/schedules/{id}/difference", method = RequestMethod.GET)
  @ResponseBody
  public String getTotalDifference(@PathVariable("id") UUID scheduleId) {
    Schedule schedule = scheduleRepository.findOne(scheduleId);

    Iterable<Period> allPeriods = periodRepository.findByProcessingSchedule(schedule);
    if (!allPeriods.equals(null)) {
      Period firstPeriod = allPeriods.iterator().next();
      Period lastPeriod = lastPeriod(allPeriods);
      java.time.Period total = java.time.Period.between(firstPeriod.getStartDate(),
              lastPeriod.getEndDate());
      String months = Integer.toString(total.getMonths());
      String days = Integer.toString(total.getDays());

      String[] msgArgs = {months, days};
      logger.debug("Returning total days and months of schedule periods");

      return messageSource.getMessage("requisition.message.totalPeriod", msgArgs,
              LocaleContextHolder.getLocale());
    } else {
      String[] messageArgs = {"0","0"};
      return messageSource.getMessage("requisition.message.totalPeriod", messageArgs,
              LocaleContextHolder.getLocale());
    }
  }

  private Period lastPeriod(Iterable<Period> iterable) {
    Period last = null;
    for (Period p : iterable) {
      last = p;
    }
    return last;
  }
}