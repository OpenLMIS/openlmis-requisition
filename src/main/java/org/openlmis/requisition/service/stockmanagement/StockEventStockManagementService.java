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

import com.fasterxml.jackson.databind.ObjectMapper;
import org.openlmis.requisition.dto.LocalizedMessageDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.exception.ExternalApiException;
import org.openlmis.requisition.exception.ServerException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.utils.RequestHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import java.io.IOException;
import java.util.UUID;

@Service
public class StockEventStockManagementService
    extends BaseStockManagementService<StockEventDto> {

  @Autowired
  private ObjectMapper objectMapper;

  /**
   * Saves the given stock event to the stockmanagement service.
   *
   * @param stockEventDto  the physical inventory draft to be saved
   * @return  the saved inventory draft
   */
  @SuppressWarnings("PMD.PreserveStackTrace")
  public UUID submit(StockEventDto stockEventDto) {
    String url = getServiceUrl() + getUrl();

    try {
      ResponseEntity<UUID> response = runWithTokenRetry(() -> restTemplate.exchange(
          createUri(url),
          HttpMethod.POST,
          RequestHelper.createEntity(authService.obtainAccessToken(), stockEventDto),
          UUID.class
      ));

      return response.getBody();
    } catch (HttpStatusCodeException ex) {
      if (ex.getStatusCode() == HttpStatus.BAD_REQUEST) {
        try {
          LocalizedMessageDto localizedMessage =
              objectMapper.readValue(ex.getResponseBodyAsString(), LocalizedMessageDto.class);

          throw new ExternalApiException(ex, localizedMessage);
        } catch (IOException ex2) {
          throw new ServerException(ex2, MessageKeys.ERROR_IO, ex2.getMessage());
        }
      } else {
        throw buildDataRetrievalException(ex);
      }
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
