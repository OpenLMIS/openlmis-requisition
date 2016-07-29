package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.springframework.beans.factory.annotation.Autowired;

public class GeographicZoneRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<GeographicZone> {

  @Autowired
  GeographicLevelRepository geographicLevelRepository;

  @Autowired
  GeographicZoneRepository repository;

  GeographicZoneRepository getRepository() {
    return this.repository;
  }

  private GeographicLevel level = new GeographicLevel();

  @Before
  public void setUp() {
    level.setCode("GeographicZoneRepositoryIntegrationTest");
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);
  }

  GeographicZone generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    GeographicZone zone = new GeographicZone();
    zone.setCode(String.valueOf(instanceNumber));
    zone.setLevel(level);
    return zone;
  }
}
