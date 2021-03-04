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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.QuoteMode;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.utils.RequisitionExportHelper;
import org.openlmis.requisition.web.RequisitionDtoBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

@Service
public class QuantificationExtractService {

  private static final String[] HEADERS = {
      "Facility Name", "Facility Code", "Product Name", "Product Code", "Unit",
      "Adjusted Consumption"
  };

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private RequisitionExportHelper exportHelper;

  /**
   * Extract the quantification data from requisitions to CSV. Return a ByteArrayInputStream
   */
  public ByteArrayInputStream extractToCsv(Page<Requisition> requisitionDtoPage) {
    final CSVFormat format = CSVFormat.DEFAULT.withQuoteMode(QuoteMode.MINIMAL).withHeader(HEADERS);
    try {
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      CSVPrinter csvPrinter = new CSVPrinter(new PrintWriter(out), format);
      for (Requisition requisition: requisitionDtoPage) {
        List<RequisitionLineItem> requisitionLineItems = requisition.getRequisitionLineItems();
        RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);
        List<RequisitionLineItemDto> itemDtos = exportHelper.exportToDtos(requisitionLineItems);
        FacilityDto facilityDto = requisitionDto.getFacility();
        for (RequisitionLineItemDto itemDto : itemDtos) {
          OrderableDto orderableDto = itemDto.getOrderable();
          List<String> data = Arrays.asList(
              facilityDto.getName(),
              facilityDto.getCode(),
              orderableDto.getFullProductName(),
              orderableDto.getProductCode(),
              orderableDto.getDispensable().getDispensingUnit(),
              String.valueOf(itemDto.getAdjustedConsumption())
          );

          csvPrinter.printRecord(data);
        }
      }
      csvPrinter.flush();
      return new ByteArrayInputStream(out.toByteArray());
    } catch (IOException e) {
      throw new ValidationMessageException("fail to import data to CSV file: " + e.getMessage(), e);
    }
  }
}
