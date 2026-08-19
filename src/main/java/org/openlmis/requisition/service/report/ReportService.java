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

package org.openlmis.requisition.service.report;

import static org.openlmis.requisition.utils.RequestHelper.createUri;

import java.util.Map;
import org.openlmis.requisition.utils.RequestHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;

@Service
public class ReportService extends BaseReportService<byte[]> {

  private static final Logger LOGGER = LoggerFactory.getLogger(ReportService.class);

  /**
   * Sends a compiled report and its parameters to the report service, which fills it with the
   * shared translation bundle and the global header.
   *
   * @param reportName name of the report.
   * @param reportData serialized compiled report.
   * @param params report parameters, must be JSON serializable.
   * @return exported report.
   */
  public byte[] generate(String reportName, byte[] reportData, Map<String, Object> params) {
    String url = getServiceUrl() + getUrl();
    GenerateReportDto request = new GenerateReportDto(reportName, reportData, params);

    LOGGER.debug("Sending generate report request to Report: {}", reportName);

    try {
      return runWithRetryAndTokenRetry(() ->
          restTemplate.exchange(
              createUri(url),
              HttpMethod.POST,
              RequestHelper.createEntity(request, authService.obtainAccessToken()),
              byte[].class
          )).getBody();

    } catch (HttpStatusCodeException ex) {
      throw buildDataRetrievalException(ex);
    }
  }

  @Override
  protected String getUrl() {
    return "/api/reports/generate";
  }

  @Override
  protected Class<byte[]> getResultClass() {
    return byte[].class;
  }

  @Override
  protected Class<byte[][]> getArrayResultClass() {
    return byte[][].class;
  }
}
