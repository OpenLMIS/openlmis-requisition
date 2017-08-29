/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.service.stockmanagement;

import static org.openlmis.utils.RequestHelper.createUri;

import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.utils.RequestHelper;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;

@Service
public class StockEventStockManagementService
    extends BaseStockManagementService<StockEventDto> {

  /**
   * Saves the given physical inventory draft to the stockmanagement service.
   *
   * @param stockEventDto  the physical inventory draft to be saved
   * @return  the saved inventory draft
   */
  public StockEventDto save(StockEventDto stockEventDto) {
    String url = getServiceUrl() + getUrl();

    try {
      ResponseEntity<StockEventDto> response = runWithTokenRetry(() -> restTemplate.exchange(
          createUri(url),
          HttpMethod.POST,
          RequestHelper.createEntity(authService.obtainAccessToken(), stockEventDto),
          getResultClass()
      ));

      return response.getBody();
    } catch (HttpStatusCodeException ex) {
      throw buildDataRetrievalException(ex);
    }
  }

  @Override
  protected String getUrl() {
    return "/api/stockEvents";
  }

  @Override
  protected Class<StockEventDto> getResultClass() {
    return StockEventDto.class;
  }

  @Override
  protected Class<StockEventDto[]> getArrayResultClass() {
    return StockEventDto[].class;
  }
}
