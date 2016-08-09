package org.openlmis.referencedata.service;

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
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class SupplyLineServiceTest {

  private static final String CODE = "123";

  @Autowired
  private SupplyLineService supplyLineService;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private ProgramRepository programRepository;

  private SupplyLine supplyLine;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    supplyLine = generateSupplyLine();
  }

  @Test
  public void testSearchSupplyLines() {
    List<SupplyLine> receivedSupplyLines = supplyLineService.searchSupplyLines(
        supplyLine.getProgram(),
        supplyLine.getSupervisoryNode());
    Assert.assertEquals(1, receivedSupplyLines.size());
    for ( SupplyLine receivedSupplyLine : receivedSupplyLines ) {
      Assert.assertEquals(
          supplyLine.getProgram().getId(),
          receivedSupplyLine.getProgram().getId());
      Assert.assertEquals(
          supplyLine.getSupervisoryNode().getId(),
          receivedSupplyLine.getSupervisoryNode().getId());
      Assert.assertEquals(
          supplyLine.getId(),
          receivedSupplyLine.getId());
    }
  }

  private SupplyLine generateSupplyLine() {
    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setProgram(generateProgram());
    supplyLine.setSupervisoryNode(generateSupervisoryNode());
    supplyLine.setSupplyingFacility(generateFacility());
    supplyLineRepository.save(supplyLine);
    return supplyLine;
  }

  private SupervisoryNode generateSupervisoryNode() {
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("234");
    supervisoryNode.setFacility(generateFacility());
    supervisoryNodeRepository.save(supervisoryNode);
    return supervisoryNode;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode(CODE);
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }

  private Facility generateFacility() {
    Integer instanceNumber = + generateInstanceNumber();
    GeographicLevel geographicLevel = generateGeographicLevel();
    GeographicZone geographicZone = generateGeographicZone(geographicLevel);
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + generateInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + generateInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + generateInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
