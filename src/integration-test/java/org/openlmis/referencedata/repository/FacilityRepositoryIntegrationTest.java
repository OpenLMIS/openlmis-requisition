package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.springframework.beans.factory.annotation.Autowired;

public class FacilityRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Facility> {

  @Autowired
  FacilityRepository repository;

  FacilityRepository getRepository() {
    return this.repository;
  }

  private FacilityType facilityType = new FacilityType();
  private GeographicZone geographicZone = new GeographicZone();

  /** Create related instances. */
  @Before
  public void setUp() {
    this.facilityType.setCode("FacilityRepositoryIntegrationTest");
    GeographicLevel level = new GeographicLevel();
    level.setCode("FacilityRepositoryIntegrationTest");
    level.setLevelNumber(1);
    this.geographicZone.setCode("FacilityRepositoryIntegrationTest");
    this.geographicZone.setLevel(level);
  }

  Facility generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Facility facility = new Facility();
    facility.setType(this.facilityType);
    facility.setGeographicZone(this.geographicZone);
    facility.setCode("F" + instanceNumber);
    facility.setName("Facility #" + instanceNumber);
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
    return facility;
  }
}
