package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.referencedata.service.PeriodService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

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

  @Autowired
  private PeriodService periodService;

  /**
   * Allows creating new schedules.
   *
   * @param schedule A schedule bound to the request body
   * @return ResponseEntity containing the created schedule
   */
  @RequestMapping(value = "/schedules", method = RequestMethod.POST)
  public ResponseEntity<?> createSchedule(@RequestBody Schedule schedule) {
    if (schedule == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Creating new schedule");
      // Ignore provided id
      schedule.setId(null);
      Schedule newSchedule = scheduleRepository.save(schedule);
      return new ResponseEntity<Schedule>(newSchedule, HttpStatus.CREATED);
    }
  }

  /**
   * Allows deleting schedule.
   *
   * @param scheduleId UUID of schedule whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/schedules/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteSchedule(@PathVariable("id") UUID scheduleId) {
    Schedule schedule = scheduleRepository.findOne(scheduleId);
    if (schedule == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Deleting choosen schedule");
      scheduleRepository.delete(schedule);
      return new ResponseEntity<Stock>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Calculates total difference in days and months
   *      between schedule beginning and end.
   *
   * @param scheduleId UUID of given Schedule.
   * @return String which contains information about total difference
   *      between schedule beginning and end.
   */
  @RequestMapping(value = "/schedules/{id}/difference", method = RequestMethod.GET)
  @ResponseBody
  public String getTotalDifference(@PathVariable("id") UUID scheduleId) {
    Schedule schedule = scheduleRepository.findOne(scheduleId);

    Iterable<Period> allPeriods = periodService.searchPeriods(schedule, null);
    if (!allPeriods.equals(null)) {
      Period firstPeriod = allPeriods.iterator().next();
      Period lastPeriod = periodRepository.findFirst1ByOrderByEndDateDesc();
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
}