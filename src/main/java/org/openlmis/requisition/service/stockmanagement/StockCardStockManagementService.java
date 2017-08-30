package org.openlmis.requisition.service.stockmanagement;

import org.openlmis.requisition.dto.stockmanagement.StockCardDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
public class StockCardStockManagementService extends BaseStockManagementService<StockCardDto> {

  public List<StockCardDto> getStockCards(UUID facility, UUID program) {
    RequestParameters params = RequestParameters.init()
        .set("pageSize", Integer.MAX_VALUE)
        .set("facility", facility.toString())
        .set("program", program.toString());

    return getPage("", params).getContent();
  }

  @Override
  protected String getUrl() {
    return "/api/stockCardSummaries";
  }

  @Override
  protected Class<StockCardDto> getResultClass() {
    return StockCardDto.class;
  }

  @Override
  protected Class<StockCardDto[]> getArrayResultClass() {
    return StockCardDto[].class;
  }
}
