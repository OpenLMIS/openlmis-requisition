package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.time.LocalDate;
import org.junit.Assert;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.junit.Test;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class PeriodControllerIntegrationTest {

  private static final String RESOURCE_URL = "http://localhost:8080/api/periods";
  private static final String SCHEDULE_URL = "http://localhost:8080/api/schedules";

  private Period firstPeriod = new Period();
  private Period secondPeriod = new Period();
  private Schedule schedule = new Schedule();

  @Before
  public void setUp() {
    schedule.setCode("code");
    schedule.setName("schedule");
    schedule.setDescription("Test schedule");
    firstPeriod.setName("period");
    firstPeriod.setDescription("Test period");
    firstPeriod.setStartDate(LocalDate.of(2016, 1, 1));
    firstPeriod.setEndDate(LocalDate.of(2016, 2, 1));
    secondPeriod.setName("period");
    secondPeriod.setDescription("Test period");
    secondPeriod.setStartDate(LocalDate.of(2016, 2, 2));
    secondPeriod.setEndDate(LocalDate.of(2016, 3, 2));
  }

  @Test
  public void testCreatePeriodsWithoutGap() throws JsonProcessingException {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    RestTemplate restTemplate = new RestTemplate();

    ObjectMapper mapper = new ObjectMapper();

    String scheduleJson = mapper.writeValueAsString(schedule);
    HttpEntity<String> scheduleEntity = new HttpEntity<>(scheduleJson, headers);

    ResponseEntity<Schedule> scheduleResult = restTemplate.postForEntity(
            SCHEDULE_URL, scheduleEntity, Schedule.class);

    schedule.setId(scheduleResult.getBody().getId());
    firstPeriod.setProcessingSchedule(schedule);

    String firstPeriodJson = mapper.writeValueAsString(firstPeriod);

    HttpEntity<String> periodEntity = new HttpEntity<>(firstPeriodJson, headers);

    restTemplate.postForEntity(RESOURCE_URL, periodEntity, Period.class);

    secondPeriod.setProcessingSchedule(schedule);

    String secondPeriodJson = mapper.writeValueAsString(secondPeriod);

    HttpEntity<String> secondPeriodEntity = new HttpEntity<>(secondPeriodJson, headers);

    ResponseEntity<Period> result = restTemplate.postForEntity(
            RESOURCE_URL, secondPeriodEntity, Period.class);

    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());
    Period savedPeriod = result.getBody();
    Assert.assertNotNull(savedPeriod.getId());
  }

  @Test(expected = HttpClientErrorException.class)
  public void testCreatePeriodsWithAGap() throws JsonProcessingException {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    RestTemplate restTemplate = new RestTemplate();

    ObjectMapper mapper = new ObjectMapper();

    schedule.setCode("newCode");
    schedule.setName("newSchedule");

    String scheduleJson = mapper.writeValueAsString(schedule);
    HttpEntity<String> scheduleEntity = new HttpEntity<>(scheduleJson, headers);

    ResponseEntity<Schedule> scheduleResult = restTemplate.postForEntity(
            SCHEDULE_URL, scheduleEntity, Schedule.class);

    schedule.setId(scheduleResult.getBody().getId());
    firstPeriod.setProcessingSchedule(schedule);

    String firstPeriodJson = mapper.writeValueAsString(firstPeriod);

    HttpEntity<String> periodEntity = new HttpEntity<>(firstPeriodJson, headers);

    restTemplate.postForEntity(RESOURCE_URL, periodEntity, Period.class);

    secondPeriod.setStartDate(LocalDate.of(2016, 2, 3));
    secondPeriod.setEndDate(LocalDate.of(2016, 3, 2));
    secondPeriod.setProcessingSchedule(schedule);

    String secondPeriodJson = mapper.writeValueAsString(secondPeriod);

    HttpEntity<String> secondPeriodEntity = new HttpEntity<>(secondPeriodJson, headers);

    restTemplate.postForEntity(RESOURCE_URL, secondPeriodEntity, Period.class);
  }
}
