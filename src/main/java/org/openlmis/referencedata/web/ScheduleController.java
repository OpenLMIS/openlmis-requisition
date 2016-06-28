package org.openlmis.referencedata.web;

import java.util.UUID;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


@RepositoryRestController
public class ScheduleController {
    private Logger logger = LoggerFactory.getLogger(ScheduleController.class);

    @Autowired
    private ScheduleRepository scheduleRepository;

    @Autowired
    private PeriodRepository periodRepository;

    @RequestMapping(value = "/schedules", method = RequestMethod.POST)
    public ResponseEntity<?> createSchedule(@RequestBody Schedule schedule) {
        if (schedule == null) {
            return new ResponseEntity(HttpStatus.BAD_REQUEST);
        } else {
            logger.debug("Creating new schedule");
            Schedule newSchedule = scheduleRepository.save(schedule);
            return new ResponseEntity<Schedule>(newSchedule, HttpStatus.CREATED);
        }
    }


    @RequestMapping(value = "/schedules/{id}/difference", method = RequestMethod.GET)
    public ResponseEntity<String> getTotalDifference(@PathVariable("id") UUID scheduleId){
        Schedule schedule = scheduleRepository.findOne(scheduleId);
        Iterable<Period> allPeriods = periodRepository.findByProcessingSchedule(schedule);
        Period firstPeriod = allPeriods.iterator().next();
        Period lastPeriod = allPeriods.iterator().next();
        while(allPeriods.iterator().hasNext()) {
            lastPeriod = allPeriods.iterator().next();
        }
        java.time.Period p = java.time.Period.between(firstPeriod.getStartDate(), lastPeriod.getEndDate());

        return new ResponseEntity<String>("Period lasts "+p.getMonths()+" months and "+p.getDays()+" days", HttpStatus.OK);
    }
}
