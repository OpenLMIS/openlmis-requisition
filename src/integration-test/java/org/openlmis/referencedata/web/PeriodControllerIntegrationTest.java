package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.time.LocalDate;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
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
import org.springframework.web.client.RestTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class PeriodControllerIntegrationTest {

    private static final String RESOURCE_URL = "http://localhost:8080/api/periods";
    private static final String SCHEDULE_URL = "http://localhost:8080/api/schedules";

    private Period period = new Period();
    private Schedule schedule = new Schedule();

    @Before
    public void setUp() {
        schedule.setCode("code");
        schedule.setName("schedule");
        schedule.setDescription("Test schedule");
        period.setName("period");
        period.setProcessingSchedule(schedule);
        period.setDescription("Test period");
        period.setStartDate(LocalDate.of(2016, 1, 1));
        period.setEndDate(LocalDate.of(2016, 2, 1));
    }

    @Test
    public void testCreatePeriodsWithoutGap() throws JsonProcessingException {
        RestTemplate restTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        ObjectMapper mapper = new ObjectMapper();

        String scheduleJson = mapper.writeValueAsString(schedule);
        HttpEntity<String> scheduleEntity = new HttpEntity<>(scheduleJson, headers);

        ResponseEntity<Schedule> scheduleResult = restTemplate.postForEntity(
                SCHEDULE_URL, scheduleEntity, Schedule.class);

        String json = mapper.writeValueAsString(period);
        HttpEntity<String> entity = new HttpEntity<>(json, headers);

        ResponseEntity<Period> result = restTemplate.postForEntity(
                RESOURCE_URL, entity, Period.class);

        period.setStartDate(LocalDate.of(2016, 2, 2));
        period.setEndDate(LocalDate.of(2016, 3, 2));
        ResponseEntity<Period> result2 = restTemplate.postForEntity(
                RESOURCE_URL, entity, Period.class);

        Period savedPeriod = result.getBody();
        Period savedPeriod2 = result2.getBody();

        Assert.assertNotNull(savedPeriod.getId());
        Assert.assertNull(savedPeriod2.getId());
    }

    @Test
    public void testCreatePeriodsWithAGap() throws JsonProcessingException {
        RestTemplate restTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        ObjectMapper mapper = new ObjectMapper();
        String json = mapper.writeValueAsString(period);
        HttpEntity<String> entity = new HttpEntity<>(json, headers);

        ResponseEntity<Period> result = restTemplate.postForEntity(
                RESOURCE_URL, entity, Period.class);

        period.setStartDate(LocalDate.of(2016, 2, 3));
        period.setEndDate(LocalDate.of(2016, 3, 3));
        ResponseEntity<Period> result2 = restTemplate.postForEntity(
                RESOURCE_URL, entity, Period.class);

        Assert.assertEquals(HttpStatus.CREATED, result2.getStatusCode());

        Period savedPeriod = result.getBody();

        Assert.assertNull(savedPeriod.getId());
    }

}
