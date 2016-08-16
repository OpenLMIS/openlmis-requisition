package org.openlmis.referencedata.service;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@Transactional
@SuppressWarnings("PMD.TooManyMethods")
public class SupplyLineServiceTest {

  private List<SupplyLine> supplyLines;
  private Integer currentInstanceNumber;

  @Mock
  private SupplyLineRepository supplyLineRepository;

  @InjectMocks
  @Autowired
  private SupplyLineService supplyLineService;

  @Before
  public void setUp() {
    supplyLines = new ArrayList<>();
    currentInstanceNumber = 0;
    generateInstances();
    initMocks(this);
    mockRepositories();
  }

  @Test
  public void testSearchSupplyLines() {
    List<SupplyLine> receivedSupplyLines = supplyLineService.searchSupplyLines(
        supplyLines.get(0).getProgram(),
        supplyLines.get(0).getSupervisoryNode());

    Assert.assertEquals(1, receivedSupplyLines.size());
    Assert.assertEquals(
        supplyLines.get(0).getProgram().getId(),
        receivedSupplyLines.get(0).getProgram().getId());
    Assert.assertEquals(
        supplyLines.get(0).getSupervisoryNode().getId(),
        receivedSupplyLines.get(0).getSupervisoryNode().getId());
    Assert.assertEquals(
        supplyLines.get(0).getId(),
        receivedSupplyLines.get(0).getId());
  }

  private void generateInstances() {
    for (int instancesCount = 0; instancesCount < 5; instancesCount++) {
      supplyLines.add(generateSupplyLine());
    }
  }

  private SupplyLine generateSupplyLine() {
    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setId(UUID.randomUUID());
    supplyLine.setProgram(generateProgram());
    supplyLine.setSupervisoryNode(generateSupervisoryNode());
    supplyLine.setSupplyingFacility(generateFacility());
    return supplyLine;
  }

  private SupervisoryNode generateSupervisoryNode() {
    Integer instanceNumber = generateInstanceNumber();
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setCode("SupNodeCode" + instanceNumber);
    supervisoryNode.setFacility(generateFacility());
    return supervisoryNode;
  }

  private Program generateProgram() {
    Integer instanceNumber = generateInstanceNumber();
    Program program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode("ProgramCode" + instanceNumber);
    program.setPeriodsSkippable(false);
    return program;
  }

  private Facility generateFacility() {
    Integer instanceNumber = generateInstanceNumber();
    GeographicLevel geographicLevel = generateGeographicLevel();
    GeographicZone geographicZone = generateGeographicZone(geographicLevel);
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setId(UUID.randomUUID());
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    Integer instanceNumber = generateInstanceNumber();
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setId(UUID.randomUUID());
    geographicLevel.setCode("GeographicLevelCode" + instanceNumber);
    geographicLevel.setLevelNumber(1);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    Integer instanceNumber = generateInstanceNumber();
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setId(UUID.randomUUID());
    geographicZone.setCode("GeographicZoneCode" + instanceNumber);
    geographicZone.setLevel(geographicLevel);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    Integer instanceNumber = generateInstanceNumber();
    FacilityType facilityType = new FacilityType();
    facilityType.setId(UUID.randomUUID());
    facilityType.setCode("FacilityTypeCode" + instanceNumber);
    return facilityType;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

  private void mockRepositories() {
    for (SupplyLine supplyLine : supplyLines) {
      List<SupplyLine> matchedSupplyLines = new ArrayList<>();
      for (SupplyLine supLine : supplyLines) {
        if (supLine.getProgram().getId()
                .equals(supplyLine.getProgram().getId())
            || supLine.getSupervisoryNode().getId()
                .equals(supplyLine.getSupervisoryNode().getId())) {
          matchedSupplyLines.add(supLine);
        }
      }
      when(supplyLineRepository
          .searchSupplyLines(supplyLine.getProgram(), supplyLine.getSupervisoryNode()))
          .thenReturn(matchedSupplyLines);
      when(supplyLineRepository
          .findOne(supplyLine.getId()))
          .thenReturn(supplyLine);
      when(supplyLineRepository
          .save(supplyLine))
          .thenReturn(supplyLine);
    }
  }
}
