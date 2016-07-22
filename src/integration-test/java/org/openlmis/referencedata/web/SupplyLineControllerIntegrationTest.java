package org.openlmis.referencedata.web;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public class SupplyLineControllerIntegrationTest {

  private static final String BASE_URL = System.getenv("BASE_URL");
  private static final String SUPPLY_LINE = BASE_URL + "/api/supplyLines";

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  private SupplyLine supplyLine = new SupplyLine();
  private Program program2 = new Program();

  @Before
  public void setUp() {
    cleanup();

    program2.setCode("program2Code");
    program2.setPeriodsSkippable(true);
    programRepository.save(program2);

    Program program = new Program();
    program.setCode("programCode");
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("facilityTypeCode");
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode("levelCode");
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("zoneCode");
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("facilityCode");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("nodeCode");
    supervisoryNode.setName("name");
    supervisoryNode.setSupervisorCount(2);
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);

    supplyLine.setDescription("supplyLineDescription");
    supplyLine.setProgram(program);
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setSupplyingFacility(facility);
  }

  @After
  public void cleanup() {
    supplyLineRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    programRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
  }

  @Test
  public void testCreate() {
    Assert.assertFalse(supplyLineRepository.findAll().iterator().hasNext());

    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<SupplyLine> result = restTemplate.postForEntity(
        SUPPLY_LINE, supplyLine, SupplyLine.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    SupplyLine savedSupplyLine = result.getBody();
    Assert.assertNotNull(savedSupplyLine.getId());
    Assert.assertTrue(supplyLineRepository.findAll().iterator().hasNext());
  }

  @Test
  public void testUpdate() {
    Assert.assertFalse(supplyLineRepository.findAll().iterator().hasNext());
    supplyLine = supplyLineRepository.save(supplyLine);

    supplyLine.setDescription("newDescription");
    supplyLine.setProgram(program2);

    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<SupplyLine> result = restTemplate.postForEntity(
        SUPPLY_LINE, supplyLine, SupplyLine.class);

    Assert.assertEquals(HttpStatus.OK, result.getStatusCode());

    SupplyLine savedSupplyLine = result.getBody();
    Assert.assertEquals(supplyLine.getId(), savedSupplyLine.getId());

    Assert.assertEquals("newDescription", savedSupplyLine.getDescription());
    Assert.assertEquals(program2.getId(), savedSupplyLine.getProgram().getId());
  }
}
