package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.Ignore;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Date;
import java.util.UUID;

public class PeriodRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Period>{

    @Autowired
    PeriodRepository periodRepository;

    @Autowired
    ScheduleRepository scheduleRepository;

    PeriodRepository getRepository() {
        return this.periodRepository;
    }

    private Period period = new Period();
    private Schedule schedule = new Schedule();

    @Before
    public void setUp() {
        scheduleRepository.deleteAll();
        schedule.setCode("code");
        schedule.setName("schedule");
        schedule.setDescription("Test schedule");
        scheduleRepository.save(schedule);
    }

    Period generateInstance() {
        int instanceNumber = this.getNextInstanceNumber();
        period.setName("period" + instanceNumber);
        period.setProcessingSchedule(schedule);
        period.setDescription("Test period");
        period.setStartDate(java.sql.Date.valueOf("2013-09-04"));
        period.setEndDate(java.sql.Date.valueOf("2014-09-04"));
        return period;
    }

    @Ignore
    @Test
    public void testGetAllPeriods() {
        periodRepository.save(period);
        Iterable<Period> result = periodRepository.findAll();
        Assert.assertEquals(1, countSizeOfIterable(result));
    }

    @Ignore
    @Test
    public void testPeriodEdit() {
        UUID id = period.getId();
        period = periodRepository.findOne(id);
        String description = "New test description";
        Assert.assertNotEquals(description, period.getDescription());
        period.setDescription(description);
        period.setStartDate(java.sql.Date.valueOf("2013-09-04"));
        period.setEndDate(java.sql.Date.valueOf("2014-09-04"));
        periodRepository.save(period);
        Assert.assertEquals(description, period.getDescription());
    }

    private int countSizeOfIterable(Iterable<Period> iterable) {
        int size = 0;
        for(Period p : iterable) size++;
        return size;
    }

}