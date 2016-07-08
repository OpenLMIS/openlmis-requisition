package org.openlmis.referencedata.repository;

import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.hierarchyandsupervision.repository.RightRepository;
import org.springframework.beans.factory.annotation.Autowired;

public class RightRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<Right> {

  @Autowired
  private RightRepository repository;

  RightRepository getRepository() {
    return this.repository;
  }

  Right generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Right right = new Right();
    right.setName(String.valueOf(instanceNumber));
    right.setRightType(String.valueOf(instanceNumber));
    return right;
  }
}
