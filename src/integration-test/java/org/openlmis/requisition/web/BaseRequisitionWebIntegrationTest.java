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
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.lang3.RandomStringUtils;
import org.junit.Before;
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
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.service.referencedata.TogglzReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.ProcessingPeriodDtoDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.springframework.boot.test.mock.mockito.MockBean;

public abstract class BaseRequisitionWebIntegrationTest extends BaseWebIntegrationTest {

  @MockBean
  RequisitionDtoBuilder requisitionDtoBuilder;

  @MockBean
  private StockEventBuilder stockEventBuilder;

  @MockBean
  private StockEventStockManagementService stockEventStockManagementService;

  @MockBean
  private TogglzReferenceDataService togglzReferenceDataService;

  @Before
  public void setUp() {
    given(togglzReferenceDataService.findAll()).willReturn(emptyList());
  }

  void mockRequisitionDtoBuilderResponses() {
    given(requisitionDtoBuilder.build(any(Requisition.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder
        .build(any(Requisition.class), any(FacilityDto.class), any(ProgramDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder
        .build(any(Requisition.class), anyMapOf(VersionIdentityDto.class, OrderableDto.class),
            any(FacilityDto.class), any(ProgramDto.class), any(ProcessingPeriodDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.buildBatch(any(Requisition.class), any(FacilityDto.class),
        anyMapOf(VersionIdentityDto.class, OrderableDto.class), any(ProcessingPeriodDto.class)))
        .willAnswer(new BuildRequisitionDtoAnswer());
    given(requisitionDtoBuilder.build(anyListOf(Requisition.class)))
        .willAnswer(new BuildListOfRequisitionDtosAnswer());
  }

  void mockStockEventServiceResponses() {
    when(stockEventBuilder.fromRequisition(any(), any(), anyMap()))
        .thenReturn(new StockEventDto());
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
            OrderableDto orderableDto = new OrderableDtoDataBuilder()
                .withId(line.getOrderable().getId())
                .withVersionId(line.getOrderable().getVersionId())
                .withProgramOrderable(line.getRequisition().getProgramId(), true)
                .withProductCode(RandomStringUtils.randomAlphanumeric(5))
                .withFullProductName(RandomStringUtils.randomAlphanumeric(5))
                .buildAsDto();

            RequisitionLineItemDto lineDto = new RequisitionLineItemDto();
            line.export(lineDto, orderableDto);

            return lineDto;
          })
          .collect(Collectors.toList()));

      FacilityDto facility = null;
      if (requisition.getFacilityId() != null) {
        facility = new FacilityDtoDataBuilder()
            .withId(requisition.getFacilityId())
            .buildAsDto();
      }

      ProgramDto program = null;
      if (requisition.getProgramId() != null) {
        program = new ProgramDtoDataBuilder()
            .withId(requisition.getProgramId())
            .buildAsDto();
      }

      ProcessingPeriodDto period = null;
      if (requisition.getProcessingPeriodId() != null) {
        period = new ProcessingPeriodDtoDataBuilder()
            .withId(requisition.getProcessingPeriodId())
            .buildAsDto();
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
