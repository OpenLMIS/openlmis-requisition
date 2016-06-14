package org.openlmis.referencedata.web;

import com.google.gson.Gson;

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

  /** Create related instances. */
  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityControllerIntegrationTest");
    GeographicLevel level = new GeographicLevel();
    level.setCode("FacilityControllerIntegrationTest");
    level.setLevelNumber(1);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("FacilityControllerIntegrationTest");
    geographicZone.setLevel(level);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityControllerIntegrationTest");
    facility.setName("FacilityControllerIntegrationTest");
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
  }

  @Test
  public void testCreate() {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    Gson gson = new Gson();
    String json = gson.toJson(facility);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Facility> result = restTemplate.postForEntity(
        RESOURCE_URL, entity, Facility.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Facility savedFacility = result.getBody();

    Assert.assertNotNull(savedFacility.getId());
  }
}
