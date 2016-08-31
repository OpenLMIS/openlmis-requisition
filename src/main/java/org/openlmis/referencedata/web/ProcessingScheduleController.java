package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;
import org.openlmis.referencedata.repository.ProcessingScheduleRepository;
import org.openlmis.referencedata.service.ProcessingPeriodService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

@Controller
public class ProcessingScheduleController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProcessingScheduleController.class);

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

  @Autowired
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ExposedMessageSource messageSource;

  @Autowired
  private ProcessingPeriodService periodService;

  /**
   * Allows creating new processingSchedules.
   *
   * @param schedule A processingSchedule bound to the request body
   * @return ResponseEntity containing the created processingSchedule
   */
  @RequestMapping(value = "/processingSchedules", method = RequestMethod.POST)
  public ResponseEntity<?> createProcessingSchedule(@RequestBody ProcessingSchedule schedule) {
    try {
      LOGGER.debug("Creating new processingSchedule");
      // Ignore provided id
      schedule.setId(null);
      ProcessingSchedule newSchedule = scheduleRepository.save(schedule);
      LOGGER.debug("Created new processingSchedule with id: " + schedule.getId());
      return new ResponseEntity<ProcessingSchedule>(newSchedule, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating processingSchedule",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Allows updating processingSchedules.
   *
   * @param schedule A processingSchedule bound to the request body
   * @param scheduleId UUID of processingSchedule which we want to update
   * @return ResponseEntity containing the updated processingSchedule
   */
  @RequestMapping(value = "/processingSchedules/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProcessingSchedule(@RequestBody ProcessingSchedule schedule,
                                       @PathVariable("id") UUID scheduleId) {
    try {
      LOGGER.debug("Updating processingSchedule with id: " + scheduleId);
      ProcessingSchedule updatedSchedule = scheduleRepository.save(schedule);
      LOGGER.debug("Updated processingSchedule with id: " + scheduleId);
      return new ResponseEntity<ProcessingSchedule>(updatedSchedule, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating processingSchedule",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all processingSchedules.
   *
   * @return ProcessingSchedules.
   */
  @RequestMapping(value = "/processingSchedules", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllProcessingSchedules() {
    Iterable<ProcessingSchedule> schedules = scheduleRepository.findAll();
    return new ResponseEntity<>(schedules, HttpStatus.OK);
  }

  /**
   * Get chosen processingSchedule.
   *
   * @param scheduleId UUID of processingSchedule which we want to get
   * @return ProcessingSchedule.
   */
  @RequestMapping(value = "/processingSchedules/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProcessingSchedule(@PathVariable("id") UUID scheduleId) {
    ProcessingSchedule schedule = scheduleRepository.findOne(scheduleId);
    if (schedule == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(schedule, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting processingSchedule.
   *
   * @param scheduleId UUID of processingSchedule which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/processingSchedules/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProcessingSchedule(@PathVariable("id") UUID scheduleId) {
    ProcessingSchedule schedule = scheduleRepository.findOne(scheduleId);
    if (schedule == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        scheduleRepository.delete(schedule);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("ProcessingSchedule cannot be deleted"
                    + " because of existing dependencies", ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<ProcessingSchedule>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Calculates total difference in days and months
   *      between processingSchedule beginning and end.
   *
   * @param scheduleId UUID of given processingSchedule.
   * @return String which contains information about total difference
   *      between processingSchedule beginning and end.
   */
  @RequestMapping(value = "/processingSchedules/{id}/difference", method = RequestMethod.GET)
  @ResponseBody
  public String getTotalDifference(@PathVariable("id") UUID scheduleId) {
    ProcessingSchedule schedule = scheduleRepository.findOne(scheduleId);

    Iterable<ProcessingPeriod> allPeriods = periodService.searchPeriods(schedule, null);
    if (!allPeriods.equals(null)) {
      ProcessingPeriod firstPeriod = allPeriods.iterator().next();
      ProcessingPeriod lastPeriod = periodRepository.findFirst1ByOrderByEndDateDesc();
      java.time.Period total = java.time.Period.between(firstPeriod.getStartDate(),
              lastPeriod.getEndDate());
      String months = Integer.toString(total.getMonths());
      String days = Integer.toString(total.getDays());

      String[] msgArgs = {months, days};
      LOGGER.debug("Returning total days and months of schedule processingPeriods");

      return messageSource.getMessage("requisition.message.totalPeriod", msgArgs,
              LocaleContextHolder.getLocale());
    } else {
      String[] messageArgs = {"0","0"};
      return messageSource.getMessage("requisition.message.totalPeriod", messageArgs,
              LocaleContextHolder.getLocale());
    }
  }
}