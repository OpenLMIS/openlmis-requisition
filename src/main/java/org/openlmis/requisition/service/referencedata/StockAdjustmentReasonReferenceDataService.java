package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class StockAdjustmentReasonReferenceDataService
    extends BaseReferenceDataService<StockAdjustmentReasonDto> {

  @Override
  protected String getUrl() {
    return "/api/programs/";
  }

  @Override
  protected Class<StockAdjustmentReasonDto> getResultClass() {
    return StockAdjustmentReasonDto.class;
  }

  @Override
  protected Class<StockAdjustmentReasonDto[]> getArrayResultClass() {
    return StockAdjustmentReasonDto[].class;
  }

  /**
   * Retrieves all the stock adjustment reasons for a given program id.
   * @param programId the id of the program
   * @return a collection of stock adjustment reasons the user has fulfillment rights for
   */
  public Collection<StockAdjustmentReasonDto> getStockAdjustmentReasonsByProgram(UUID programId) {
    return findAll(programId + "/stockAdjustmentReasons");
  }
}
