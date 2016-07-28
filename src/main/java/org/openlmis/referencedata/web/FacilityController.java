package org.openlmis.referencedata.web;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.service.OrderService;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.time.LocalDate;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

@RepositoryRestController
public class FacilityController {
  Logger logger = LoggerFactory.getLogger(FacilityController.class);

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private OrderService orderService;

  /**
   * Return list of orders, filtered according to params, filled by certain facility.
   *
   * @param homeFacilityId UUID of facility whose list of orders we want
   * @param programId UUID of program we filter by
   * @param requestingFacilityId UUID of requesting facility we filter by
   * @param periodId UUID of period we filter by
   * @param scheduleId UUID of schedule we filter by
   * @param startDate LocalDate of start point we filter by
   * @param endDate LocalDate of end point we filter by
   * @param request HttpServletRequest object
   * @return result Iterable object with filtered orders
   */
  @RequestMapping(value = "/facilities/{id}/orders", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<Iterable<Order>> getOrderList(
          @PathVariable("id") UUID homeFacilityId,
          @RequestParam(value = "program", required = false) UUID programId,
          @RequestParam(value = "facility", required = false) UUID requestingFacilityId,
          @RequestParam(value = "period", required = false) UUID periodId,
          @RequestParam(value = "schedule", required = false) UUID scheduleId,
          @RequestParam(value = "startYear", required = false) LocalDate startDate,
          @RequestParam(value = "endYear", required = false) LocalDate endDate,
          HttpServletRequest request) {
    Program program = null;
    Facility requestingFacility = null;
    Period period = null;
    Schedule schedule = null;
    Facility homeFacility = facilityRepository.findOne(homeFacilityId);
    if (homeFacility == null) {
      return new ResponseEntity("Facility with provided id does not exist.",
          HttpStatus.BAD_REQUEST);
    }
    if (programId != null) {
      program = programRepository.findOne(programId);
    }
    if (requestingFacilityId != null) {
      requestingFacility = facilityRepository.findOne(requestingFacilityId);
    }
    if (periodId != null) {
      period = periodRepository.findOne(periodId);
    }
    if (scheduleId != null) {
      schedule = scheduleRepository.findOne(scheduleId);
    }
    Iterable<Order> orderList = orderService.searchOrders(homeFacility,
                                                          requestingFacility, program, period,
                                                          schedule, startDate, endDate);
    return new ResponseEntity<>(orderList, HttpStatus.OK);
  }
}
