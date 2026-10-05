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

package org.openlmis.requisition.service;

import java.nio.charset.StandardCharsets;
import org.springframework.http.MediaType;

/**
 * Formats a report generated from a Jasper template can be exported to.
 */
public enum ReportFormat {
  PDF("pdf", new MediaType("application", "pdf", StandardCharsets.UTF_8)),
  CSV("csv", new MediaType("text", "csv", StandardCharsets.UTF_8)),
  XLS("xls", new MediaType("application", "vnd.ms-excel", StandardCharsets.UTF_8)),
  XLSX("xlsx", new MediaType("application",
      "vnd.openxmlformats-officedocument.spreadsheetml.sheet", StandardCharsets.UTF_8)),
  HTML("html", new MediaType("text", "html", StandardCharsets.UTF_8));

  private final String extension;
  private final MediaType mediaType;

  ReportFormat(String extension, MediaType mediaType) {
    this.extension = extension;
    this.mediaType = mediaType;
  }

  /**
   * Returns the format with the given extension, or PDF if there is none.
   */
  public static ReportFormat fromString(String value) {
    for (ReportFormat format : values()) {
      if (format.extension.equals(value)) {
        return format;
      }
    }
    return PDF;
  }

  public String getExtension() {
    return extension;
  }

  public MediaType getMediaType() {
    return mediaType;
  }
}
