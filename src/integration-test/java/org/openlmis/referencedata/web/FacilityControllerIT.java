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
public class FacilityControllerIT {

  private final static String RESOURCE_URL = "http://localhost:8080/api/facilities";

  private Facility facility = new Facility();

  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    GeographicZone geographicZone = new GeographicZone();
    facilityType.setCode("FacilityControllerIT");
    GeographicLevel level = new GeographicLevel();
    level.setCode("FacilityControllerIT");
    level.setLevelNumber(1);
    geographicZone.setCode("FacilityControllerIT");
    geographicZone.setLevel(level);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityControllerIT");
    facility.setName("FacilityControllerIT");
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
