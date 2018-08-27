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

package org.openlmis.requisition.web;

import static java.util.Collections.emptyList;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.google.common.collect.Sets;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.RandomStringUtils;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.springframework.boot.test.mock.mockito.MockBean;

public abstract class BaseRequisitionWebIntegrationTest extends BaseWebIntegrationTest {

  @MockBean
  RequisitionDtoBuilder requisitionDtoBuilder;

  @MockBean
  private StockEventBuilder stockEventBuilder;

  @MockBean
  private StockEventStockManagementService stockEventStockManagementService;

  void mockRequisitionDtoBuilderResponses() {
    given(requisitionDtoBuilder.build(any(Requisition.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder
        .build(any(Requisition.class), any(FacilityDto.class), any(ProgramDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder
        .build(any(Requisition.class), anyMapOf(UUID.class, OrderableDto.class),
            any(FacilityDto.class), any(ProgramDto.class), any(ProcessingPeriodDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.buildBatch(any(Requisition.class), any(FacilityDto.class),
        anyMapOf(UUID.class, OrderableDto.class), any(ProcessingPeriodDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.build(anyListOf(Requisition.class)))
        .willAnswer(new BuildListOfRequisitionDtosAnswer());
  }

  void mockStockEventServiceResponses() {
    when(stockEventBuilder.fromRequisition(any(), any())).thenReturn(new StockEventDto());
    doNothing().when(stockEventStockManagementService).submit(any(StockEventDto.class));
  }

  void mockRepositorySaveAnswer() {
    given(requisitionRepository.save(any(Requisition.class))).willAnswer(new SaveAnswer<>());
  }

  private static class BuildRequisitionDtoAnswer implements Answer<RequisitionDto> {

    @Override
    public RequisitionDto answer(InvocationOnMock invocation) throws Throwable {
      Requisition requisition = (Requisition) invocation.getArguments()[0];

      if (null == requisition) {
        return null;
      }

      return export(requisition);
    }

    private static RequisitionDto export(Requisition requisition) {
      RequisitionDto dto = new RequisitionDto();
      requisition.export(dto);

      dto.setTemplate(BasicRequisitionTemplateDto.newInstance(requisition.getTemplate()));
      dto.setRequisitionLineItems(Optional
          .ofNullable(requisition.getRequisitionLineItems())
          .orElse(emptyList())
          .stream()
          .map(line -> {
            OrderableDto orderableDto = new OrderableDto();
            orderableDto.setId(line.getOrderableId());
            orderableDto.setPrograms(Sets.newHashSet());
            orderableDto.setProductCode(RandomStringUtils.randomAlphanumeric(5));
            orderableDto.setFullProductName(RandomStringUtils.randomAlphanumeric(5));

            RequisitionLineItemDto lineDto = new RequisitionLineItemDto();
            line.export(lineDto, orderableDto);

            return lineDto;
          })
          .collect(Collectors.toList()));

      FacilityDto facility = null;
      if (requisition.getFacilityId() != null) {
        facility = new FacilityDto();
        facility.setId(requisition.getFacilityId());
        facility.setName("facility");
      }

      ProgramDto program = null;
      if (requisition.getProgramId() != null) {
        program = new ProgramDto();
        program.setId(requisition.getProgramId());
        program.setName("program");
      }

      ProcessingPeriodDto period = null;
      if (requisition.getProcessingPeriodId() != null) {
        period = new ProcessingPeriodDto();
        period.setId(requisition.getProcessingPeriodId());
        period.setName("period");
      }

      dto.setProcessingPeriod(period);
      dto.setFacility(facility);
      dto.setProgram(program);

      return dto;
    }
  }

  private static class BuildListOfRequisitionDtosAnswer implements Answer<List<RequisitionDto>> {

    @Override
    public List<RequisitionDto> answer(InvocationOnMock invocation) {
      Collection<Requisition> collection = (Collection) invocation.getArguments()[0];

      if (null == collection) {
        return emptyList();
      }

      return collection
          .stream()
          .map(BuildRequisitionDtoAnswer::export)
          .collect(Collectors.toList());
    }
  }

}
