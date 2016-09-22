package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.OrderableProductDto;
import org.springframework.stereotype.Service;

@Service
public class OrderableProductReferenceDataService extends BaseReferenceDataService {

  @Override
  protected String getUrl() {
    return "/orderableProducts/";
  }

  @Override
  protected Class<OrderableProductDto> getResultClass() {
    return OrderableProductDto.class;
  }
}
