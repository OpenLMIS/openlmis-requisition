package org.openlmis.referencedata.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.service.SupplyLineService;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@SuppressWarnings("PMD.TooManyMethods")
public class SupplyLineServiceTest {

  private List<SupplyLine> supplyLines;
  private Integer currentInstanceNumber;

  @Mock
  private SupplyLineRepository supplyLineRepository;

  @InjectMocks
  private SupplyLineService supplyLineService;

  @Before
  public void setUp() {
    supplyLines = new ArrayList<>();
    currentInstanceNumber = 0;
    generateInstances();
    initMocks(this);
  }

  @Test
  public void testShouldFindSupplyLineIfMatchedProgramAndSupervisoryNode() {
    when(supplyLineRepository
            .searchSupplyLines(supplyLines.get(0).getProgram(),
                    supplyLines.get(0).getSupervisoryNode()))
            .thenReturn(Arrays.asList(supplyLines.get(0)));
    List<SupplyLine> receivedSupplyLines = supplyLineService.searchSupplyLines(
        supplyLines.get(0).getProgram(),
        supplyLines.get(0).getSupervisoryNode());

    assertEquals(1, receivedSupplyLines.size());
    assertEquals(
        supplyLines.get(0).getProgram().getId(),
        receivedSupplyLines.get(0).getProgram().getId());
    assertEquals(
        supplyLines.get(0).getSupervisoryNode().getId(),
        receivedSupplyLines.get(0).getSupervisoryNode().getId());
    assertEquals(
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
}
