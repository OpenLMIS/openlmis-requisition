package org.openlmis.requisition.service.fulfillment;

import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.service.BaseCommunicationServiceTest;
import org.springframework.test.util.ReflectionTestUtils;

public abstract class BaseFulfillmentServiceTest<T> extends BaseCommunicationServiceTest<T> {

  @Override
  protected BaseFulfillmentService<T> prepareService() {
    BaseCommunicationService service = super.prepareService();

    ReflectionTestUtils.setField(service, "fulfillmentUrl", "http://localhost");

    return (BaseFulfillmentService<T>) service;
  }

}
