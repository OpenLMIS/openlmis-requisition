package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
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
public class FacilityControllerIntegrationTest {

  private static final String RESOURCE_URL = "http://localhost:8080/api/facilities";

  private Facility facility = new Facility();

  private String facilityController = "FacilityControllerIntegrationTest";

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(facilityController);
    GeographicLevel level = new GeographicLevel();
    level.setCode(facilityController);
    level.setLevelNumber(1);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(facilityController);
    geographicZone.setLevel(level);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(facilityController);
    facility.setName(facilityController);
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
  }

  @Test
  public void testCreate() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(facility);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Facility> result = restTemplate.postForEntity(
        RESOURCE_URL, entity, Facility.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Facility savedFacility = result.getBody();

    Assert.assertNotNull(savedFacility.getId());
  }
}
