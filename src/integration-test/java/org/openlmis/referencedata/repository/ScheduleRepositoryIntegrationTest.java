package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDateTime;

@SuppressWarnings("PMD.UnusedLocalVariable")
public class ScheduleRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Schedule> {

  @Autowired
  ScheduleRepository repository;

  @Before
  public void setUp() {
    repository.deleteAll();
    repository.save(getExampleSchedule());
  }

  @Override
  ScheduleRepository getRepository() {
    return this.repository;
  }

  @Override
  Schedule generateInstance() {
    return getExampleSchedule();
  }

  @Test
  public void testGetAllSchedules() {
    repository.save(getExampleSchedule());
    Iterable<Schedule> result = repository.findAll();

    Assert.assertEquals(2, countSizeOfIterable(result));
  }

  @Test
  public void testScheduleEdit() {
    Iterable<Schedule> iterable = repository.findAll();
    Schedule scheduleFromRepo = iterable.iterator().next();
    String newDescription = "New test description babe";
    Assert.assertNotEquals(newDescription, scheduleFromRepo.getDescription());

    scheduleFromRepo.setDescription(newDescription);
    repository.save(scheduleFromRepo);
    LocalDateTime savingDateTime = scheduleFromRepo.getModifiedDate();
    iterable = repository.findAll();
    scheduleFromRepo = iterable.iterator().next();
    Assert.assertTrue(savingDateTime.isBefore(scheduleFromRepo.getModifiedDate()));
    Assert.assertEquals(newDescription, scheduleFromRepo.getDescription());
  }

  private Schedule getExampleSchedule() {
    int instanceNumber = this.getNextInstanceNumber();
    Schedule schedule = new Schedule();
    schedule.setCode("code" + instanceNumber);
    schedule.setName("schedule#" + instanceNumber);
    schedule.setDescription("Test schedule");
    return schedule;
  }

  private int countSizeOfIterable(Iterable<Schedule> iterable) {
    int size = 0;
    for (Schedule s : iterable) {
      size++;
    }
    return size;
  }
}
