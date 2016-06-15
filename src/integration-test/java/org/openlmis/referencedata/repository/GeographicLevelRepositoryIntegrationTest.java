package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.GeographicLevel;
import org.springframework.beans.factory.annotation.Autowired;

public class GeographicLevelRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<GeographicLevel> {

  @Autowired
  GeographicLevelRepository repository;

  GeographicLevelRepository getRepository() {
    return this.repository;
  }

  GeographicLevel generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    GeographicLevel level = new GeographicLevel();
    level.setCode(String.valueOf(instanceNumber));
    level.setLevelNumber(instanceNumber);
    return level;
  }
}
