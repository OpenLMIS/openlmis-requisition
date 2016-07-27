package org.openlmis.referencedata.repository;

import org.junit.Assert;
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

import java.util.UUID;

public class SupplyLineRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<SupplyLine> {

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
  private SupplyLineRepository repository;

  private Program program = new Program();
  private SupervisoryNode supervisoryNode = new SupervisoryNode();

  @Override
  CrudRepository<SupplyLine, UUID> getRepository() {
    return repository;
  }

  @Override
  SupplyLine generateInstance() {
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

    supervisoryNode.setCode("nodeCode");
    supervisoryNode.setName("name");
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);

    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setDescription("supplyLineDescription");
    supplyLine.setProgram(program);
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setSupplyingFacility(facility);

    return supplyLine;
  }

  @Test
  public void findByProgramAndSupervisoryNode() {
    repository.save(generateInstance());
    SupplyLine supplyLine =
        repository.findByProgramAndSupervisoryNode(program, supervisoryNode);
    Assert.assertNotNull(supplyLine);
  }
}
