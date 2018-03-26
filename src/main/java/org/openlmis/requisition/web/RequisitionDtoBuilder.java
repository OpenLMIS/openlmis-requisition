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

import static org.openlmis.requisition.dto.ReasonDto.newInstance;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.BatchApproveRequisitionDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.utils.RequisitionExportHelper;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RequisitionDtoBuilder {
  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(RequisitionDtoBuilder.class);

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  /**
   * Create a list of {@link RequisitionDto} based on passed data.
   *
   * @param requisitions a list of requisitions that will be converted into DTOs.
   * @return a list of {@link RequisitionDto}
   */
  public List<RequisitionDto> build(Collection<Requisition> requisitions) {
    return requisitions.stream().map(this::build).collect(Collectors.toList());
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public RequisitionDto build(Requisition requisition) {
    XLOGGER.entry(requisition);
    Profiler profiler = new Profiler("REQUISITION_DTO_BUILD_WITHOUT_FACILITY_PROGRAM");
    profiler.setLogger(XLOGGER);

    profiler.start("GET_FACILITY");
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());

    profiler.start("GET_PROGRAM");
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());

    profiler.start("CALL_REQUISITION_DTO_BUILD");
    RequisitionDto requisitionDto = build(requisition, facility, program);

    profiler.stop().log();
    XLOGGER.exit(requisitionDto);
    return requisitionDto;
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public RequisitionDto build(Requisition requisition, FacilityDto facility,
                              ProgramDto program) {
    XLOGGER.entry(requisition, facility, program);
    if (null == requisition) {
      XLOGGER.exit();
      return null;
    }
    Profiler profiler = new Profiler("REQUISITION_DTO_BUILD_WITHOUT_ORDERABLES");
    profiler.setLogger(XLOGGER);

    profiler.start("GET_ORDERABLES");
    Map<UUID, OrderableDto> orderables = orderableReferenceDataService
        .findByIds(requisition.getAllOrderableIds())
        .stream()
        .collect(Collectors.toMap(OrderableDto::getId, Function.identity()));

    profiler.start("CALL_REQUISITION_DTO_BUILD");
    RequisitionDto requisitionDto = build(requisition, orderables, facility, program);

    profiler.stop().log();
    XLOGGER.exit(requisitionDto);
    return requisitionDto;
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public RequisitionDto build(Requisition requisition, Map<UUID, OrderableDto> orderables,
      FacilityDto facility, ProgramDto program) {
    XLOGGER.entry(requisition, facility, program);
    if (null == requisition) {
      XLOGGER.exit();
      return null;
    }

    Profiler profiler = new Profiler("REQUISITION_DTO_BUILD");
    profiler.setLogger(XLOGGER);

    RequisitionDto requisitionDto = new RequisitionDto();

    profiler.start("EXPORT");
    requisition.export(requisitionDto);

    profiler.start("SET_SUB_RESOURCES");
    setSubResources(requisition, facility, null, requisitionDto, program);

    profiler.start("GET_LINE_ITEMS");
    List<RequisitionLineItem> requisitionLineItems = requisition.getRequisitionLineItems();

    profiler.start("EXPORT_LINE_ITEMS_TO_DTOS");
    List<RequisitionLineItemDto> requisitionLineItemDtoList =
        requisitionExportHelper.exportToDtos(requisitionLineItems, orderables, false);

    profiler.start("SET_LINE_ITEMS");
    requisitionDto.setRequisitionLineItems(requisitionLineItemDtoList);

    if (requisition.getAvailableProducts() != null) {
      profiler.start("SET_AVAILABLE_PRODUCTS");
      setAvailableProductsDto(requisitionDto, requisition, orderables.values());
    }

    profiler.start("SET_STOCK_ADJ_REASONS");
    requisitionDto.setStockAdjustmentReasons(newInstance(requisition.getStockAdjustmentReasons()));

    profiler.stop().log();
    XLOGGER.exit(requisitionDto);
    return requisitionDto;
  }

  /**
   * Create a new instance of RequisitionDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link RequisitionDto} (can be {@code null})
   * @return new instance of {@link RequisitionDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public RequisitionDto buildBatch(Requisition requisition, FacilityDto facility,
                                   Map<UUID, OrderableDto> orderables,
                                   ProcessingPeriodDto period) {
    XLOGGER.entry(requisition, facility);
    if (null == requisition) {
      XLOGGER.exit();
      return null;
    }
    Profiler profiler = new Profiler("REQUISITION_DTO_BUILD");
    profiler.setLogger(XLOGGER);

    RequisitionDto requisitionDto = new BatchApproveRequisitionDto();

    requisition.export(requisitionDto);

    profiler.start("SET_SUB_RESOURCES");
    setSubResources(requisition, facility, period, requisitionDto, null);

    profiler.start("GET_LINE_ITEMS");
    List<RequisitionLineItem> requisitionLineItems = requisition.getRequisitionLineItems();

    profiler.start("EXPORT_LINE_ITEMS_TO_DTOS");
    List<RequisitionLineItemDto> requisitionLineItemDtoList =
        requisitionExportHelper.exportToDtos(requisitionLineItems, orderables, true);

    profiler.start("SET_LINE_ITEMS");
    requisitionDto.setRequisitionLineItems(requisitionLineItemDtoList);

    profiler.stop().log();
    XLOGGER.exit(requisitionDto);
    return requisitionDto;

  }

  private void setSubResources(Requisition requisition, FacilityDto facility,
                               ProcessingPeriodDto period, RequisitionDto requisitionDto,
                               ProgramDto program) {
    requisitionDto.setTemplate(
        BasicRequisitionTemplateDto.newInstance(requisition.getTemplate()));

    if (facility != null) {
      facility.setSupportedPrograms(null);
    }
    requisitionDto.setFacility(facility);
    if (period != null) {
      requisitionDto.setProcessingPeriod(period);
    } else {
      requisitionDto.setProcessingPeriod(
          periodService.getPeriod(requisition.getProcessingPeriodId()));
    }
    requisitionDto.setProgram(program);
  }

  private void setAvailableProductsDto(RequisitionDto requisitionDto, Requisition requisition,
      Collection<OrderableDto> orderables) {
    Collection<OrderableDto> localOrderables = null == orderables
        ? orderableReferenceDataService.findByIds(requisition.getAvailableProducts())
        : orderables;

    Set<OrderableDto> availableFullSupply = new HashSet<>();
    Set<OrderableDto> availableNonFullSupply = new HashSet<>();

    for (OrderableDto orderableDto : localOrderables) {
      ProgramOrderableDto poDto = orderableDto.findProgramOrderableDto(requisition.getProgramId());
      if (poDto == null) {
        continue;
      }

      if (poDto.getFullSupply()) {
        availableFullSupply.add(orderableDto);
      } else {
        availableNonFullSupply.add(orderableDto);
      }
    }

    requisitionDto.setAvailableFullSupplyProducts(availableFullSupply);
    requisitionDto.setAvailableNonFullSupplyProducts(availableNonFullSupply);
  }

}
