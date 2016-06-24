package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@RepositoryRestController
public class ScheduleController {
    private Logger logger = LoggerFactory.getLogger(ScheduleController.class);

    @Autowired
    private ScheduleRepository scheduleRepository;

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
}
