package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.repository.CrudRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class SupplyLineRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<SupplyLine> {

  @Autowired
  private SupplyLineRepository repository;

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

  private List<SupplyLine> supplyLines;

  CrudRepository<SupplyLine, UUID> getRepository() {
    return repository;
  }

  SupplyLine generateInstance() {
    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setProgram(generateProgram());
    supplyLine.setSupervisoryNode(generateSupervisoryNode());
    supplyLine.setSupplyingFacility(generateFacility());
    return supplyLine;
  }

  @Before
  public void setUp() {
    supplyLines = new ArrayList<>();
    for (int stockNumber = 0; stockNumber < 5; stockNumber++) {
      supplyLines.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchSupplyLinesByAllParameters() {
    SupplyLine supplyLine = cloneSupplyLine(supplyLines.get(0));
    List<SupplyLine> receivedSupplyLines = repository.searchSupplyLines(
            supplyLine.getProgram(), supplyLine.getSupervisoryNode());

    Assert.assertEquals(1, receivedSupplyLines.size());
    for (SupplyLine receivedSupplyLine : receivedSupplyLines) {
      Assert.assertEquals(
              supplyLine.getProgram().getId(),
              receivedSupplyLine.getProgram().getId());
      Assert.assertEquals(
              supplyLine.getSupervisoryNode().getId(),
              receivedSupplyLine.getSupervisoryNode().getId());
    }
  }

  @Test
  public void testSearchSupplyLinesByAllParametersNull() {
    List<SupplyLine> receivedSupplyLines = repository.searchSupplyLines(
            null, null);

    Assert.assertEquals(5, receivedSupplyLines.size());
  }

  @Test
  public void testSearchSupplyLinesByProgram() {
    SupplyLine supplyLine = cloneSupplyLine(supplyLines.get(0));
    List<SupplyLine> receivedSupplyLines = repository.searchSupplyLines(
            supplyLine.getProgram(), null);

    Assert.assertEquals(1, receivedSupplyLines.size());
    for (SupplyLine receivedSupplyLine : receivedSupplyLines) {
      Assert.assertEquals(
              supplyLine.getProgram().getId(),
              receivedSupplyLine.getProgram().getId());
    }
  }

  private SupplyLine cloneSupplyLine(SupplyLine supplyLine) {
    SupplyLine clonedSupplyLine = new SupplyLine();
    clonedSupplyLine.setProgram(supplyLine.getProgram());
    clonedSupplyLine.setSupervisoryNode(supplyLine.getSupervisoryNode());
    clonedSupplyLine.setDescription(supplyLine.getDescription());
    clonedSupplyLine.setSupplyingFacility(supplyLine.getSupplyingFacility());
    repository.save(clonedSupplyLine);
    return clonedSupplyLine;
  }

  private SupervisoryNode generateSupervisoryNode() {
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("code" + this.getNextInstanceNumber());
    supervisoryNode.setFacility(generateFacility());
    supervisoryNodeRepository.save(supervisoryNode);
    return supervisoryNode;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("code" + this.getNextInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }

  private Facility generateFacility() {
    Integer instanceNumber = + this.getNextInstanceNumber();
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
    geographicLevel.setCode("GeographicLevel" + this.getNextInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + this.getNextInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + this.getNextInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }
}
