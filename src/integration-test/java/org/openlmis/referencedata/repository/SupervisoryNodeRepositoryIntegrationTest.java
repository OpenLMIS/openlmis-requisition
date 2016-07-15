package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.SupervisoryNode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

public class SupervisoryNodeRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<SupervisoryNode> {
  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  private GeographicZone geographicZone = new GeographicZone();
  private GeographicLevel geographicLevel = new GeographicLevel();
  private Facility facility = new Facility();

  @Override
  CrudRepository<SupervisoryNode, UUID> getRepository() {
    return supervisoryNodeRepository;
  }

  @Before
  public void setUp() {
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();

    geographicLevel.setCode("code");
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);

    geographicZone.setCode("code");
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("code");
    facilityTypeRepository.save(facilityType);

    facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("code");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);
  }

  @Override
  SupervisoryNode generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    SupervisoryNode node = new SupervisoryNode();
    node.setCode("Code #" + instanceNumber);
    node.setName("SupervisoryNode #" + instanceNumber);
    node.setSupervisorCount(0);
    node.setFacility(facility);
    return node;
  }
}
