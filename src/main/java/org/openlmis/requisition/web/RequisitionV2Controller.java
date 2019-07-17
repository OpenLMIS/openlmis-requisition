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
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ID_MISMATCH;
import static org.openlmis.requisition.web.RequisitionV2Controller.RESOURCE_URL;
import static org.openlmis.requisition.web.ResourceNames.FACILITIES;
import static org.openlmis.requisition.web.ResourceNames.ORDERABLES;
import static org.openlmis.requisition.web.ResourceNames.PROCESSING_PERIODS;
import static org.openlmis.requisition.web.ResourceNames.PROGRAMS;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.ApprovedProductReference;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.domain.requisition.VersionEntityReference;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.MetadataDto;
import org.openlmis.requisition.dto.ObjectReferenceDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionLineItemV2Dto;
import org.openlmis.requisition.dto.RequisitionV2Dto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.dto.VersionObjectReferenceDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.openlmis.requisition.service.referencedata.ApproveProductsAggregator;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.validate.ReasonsValidator;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@Transactional
@RestController
@RequestMapping(RESOURCE_URL)
public class RequisitionV2Controller extends BaseRequisitionController {

  public static final String RESOURCE_URL = API_URL + "/v2/requisitions";

  @Autowired
  private ReasonsValidator reasonsValidator;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Value("${service.url}")
  private String serviceUrl;

  /**
   * Allows creating new requisitions.
   *
   * @param programId UUID of Program.
   * @param facilityId UUID of Facility.
   * @param emergency Emergency status.
   * @param suggestedPeriod Period for requisition.
   * @return created requisition.
   */
  @PostMapping("/initiate")
  @ResponseStatus(HttpStatus.CREATED)
  public RequisitionV2Dto initiate(@RequestParam(value = "program") UUID programId,
      @RequestParam(value = "facility") UUID facilityId,
      @RequestParam(value = "suggestedPeriod", required = false) UUID suggestedPeriod,
      @RequestParam(value = "emergency") boolean emergency,
      HttpServletRequest request, HttpServletResponse response) {

    Profiler profiler = getProfiler(
        "POST_REQUISITION_INITIATE",
        programId, facilityId, suggestedPeriod, emergency
    );

    if (null == facilityId || null == programId) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_INITIALIZE_MISSING_PARAMETERS));
    }

    checkPermission(profiler, () -> permissionService.canInitRequisition(programId, facilityId));

    validateIdempotencyKey(request, profiler);

    FacilityDto facility = findFacility(facilityId, profiler);

    profiler.start("CHECK_FACILITY_SUPPORTS_PROGRAM");
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, programId);

    profiler.start("FIND_PROCESSING_PERIOD");
    ProcessingPeriodDto period = periodService
        .findPeriod(programId, facilityId, suggestedPeriod, emergency);

    boolean reportOnly = period.isReportOnly();

    profiler.start("GET_STOCK_ADJ_REASONS");
    List<StockAdjustmentReason> stockAdjustmentReasons =
        getStockAdjustmentReasons(programId, facility);

    ProgramDto program = findProgram(programId, profiler);

    profiler.start("FIND_REQUISITION_TEMPLATE");
    RequisitionTemplate requisitionTemplate = requisitionTemplateService.findTemplate(
        program.getId(), facility.getType().getId(), reportOnly && !emergency
    );

    profiler.start("FIND_APPROVED_PRODUCTS");
    ApproveProductsAggregator approvedProducts = approvedProductReferenceDataService
        .getApprovedProducts(facility.getId(), program.getId());

    profiler.start("INITIATE_REQUISITION");
    Requisition newRequisition = requisitionService.initiate(
        program, facility, period, emergency, stockAdjustmentReasons,
        requisitionTemplate, approvedProducts);

    profiler.start("VALIDATE_REASONS");
    reasonsValidator.validate(stockAdjustmentReasons, newRequisition.getTemplate());

    Map<VersionIdentityDto, OrderableDto> orderables = findOrderables(
        profiler, newRequisition::getAllOrderables);

    RequisitionV2Dto dto = buildDto(newRequisition, orderables, profiler);

    addLocationHeader(request, response, dto.getId(), profiler);

    stopProfiler(profiler, dto);

    return dto;
  }

  /**
   * Allows updating requisitions.
   *
   * @param requisitionId UUID of requisition which we want to update.
   * @param requisitionDto A requisitionDto bound to the request body.
   * @return updated requisition.
   */
  @PutMapping("/{id}")
  public RequisitionV2Dto updateRequisition(@PathVariable("id") UUID requisitionId,
      @RequestBody RequisitionV2Dto requisitionDto,
      HttpServletRequest request, HttpServletResponse response) {
    Profiler profiler = getProfiler("UPDATE_REQUISITION", requisitionId, requisitionDto);

    if (null != requisitionDto.getId() && !Objects.equals(requisitionDto.getId(), requisitionId)) {
      throw new ValidationMessageException(ERROR_ID_MISMATCH);
    }

    Requisition requisitionToUpdate = findRequisition(requisitionId, profiler);

    profiler.start("VALIDATE_TIMESTAMPS");
    requisitionVersionValidator
        .validateRequisitionTimestamps(requisitionDto.getModifiedDate(), requisitionToUpdate)
        .throwExceptionIfHasErrors();

    checkPermission(
        profiler,
        () -> requisitionService.validateCanSaveRequisition(requisitionToUpdate)
    );

    profiler.start("VALIDATE_VERSION");
    requisitionVersionValidator.validateEtagVersionIfPresent(request, requisitionToUpdate)
        .throwExceptionIfHasErrors();

    Map<VersionIdentityDto, OrderableDto> orderables = findOrderables(
        profiler, requisitionToUpdate::getAllOrderables
    );

    profiler.start("GET_PERIOD");
    ProcessingPeriodDto period = periodService
        .getPeriod(requisitionToUpdate.getProcessingPeriodId());

    profiler.start("BUILD_REQUISITION_UPDATER");
    Map<VersionEntityReference, ApprovedProductReference> productReferences = requisitionToUpdate
        .getAvailableProducts()
        .stream()
        .collect(Collectors.toMap(ApprovedProductReference::getOrderable, Function.identity()));

    Requisition requisition = RequisitionBuilder.newRequisition(requisitionDto,
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        period, requisitionToUpdate.getStatus(), orderables, productReferences);
    requisition.setId(requisitionId);

    ProgramDto program = findProgram(requisitionToUpdate.getProgramId(), profiler);

    profiler.start("VALIDATE_CAN_BE_UPDATED");
    validateRequisitionCanBeUpdated(requisitionToUpdate, requisition, program, orderables)
        .throwExceptionIfHasErrors();

    Map<VersionIdentityDto, ApprovedProductDto> approvedProducts = findApprovedProducts(
        requisitionToUpdate::getAllApprovedProductIdentities, profiler);

    logger.debug("Updating requisition with id: {}", requisitionId);

    profiler.start("UPDATE");
    requisitionToUpdate.updateFrom(requisition, orderables, approvedProducts,
        datePhysicalStockCountCompletedEnabledPredicate.exec(program));

    profiler.start("SAVE");
    requisitionRepository.save(requisitionToUpdate);
    logger.debug("Requisition with id {} saved", requisitionToUpdate.getId());

    ETagResource<RequisitionV2Dto> etaggedResource = new ETagResource<>(
        buildDto(requisitionToUpdate, orderables, profiler),
        requisitionToUpdate.getVersion());

    response.setHeader(HttpHeaders.ETAG, etaggedResource.getEtag());

    stopProfiler(profiler, etaggedResource.getResource());

    return etaggedResource.getResource();
  }

  /**
   * Get chosen requisition.
   *
   * @param requisitionId UUID of requisition whose we want to get
   * @return Requisition.
   */
  @GetMapping("/{id}")
  public RequisitionV2Dto getRequisition(@PathVariable("id") UUID requisitionId,
      HttpServletResponse response) {
    Profiler profiler = getProfiler("GET_REQUISITION", requisitionId);

    Requisition requisition = findRequisition(requisitionId, profiler);

    checkPermission(profiler, () -> permissionService.canViewRequisition(requisition));

    Map<VersionIdentityDto, OrderableDto> orderables = findOrderables(
        profiler, requisition::getAllOrderables);

    RequisitionV2Dto dto = buildDto(requisition, orderables, profiler);
    response.setHeader(HttpHeaders.ETAG, ETagResource.buildWeakETag(requisition.getVersion()));

    stopProfiler(profiler, dto);

    return dto;
  }

  private RequisitionV2Dto buildDto(Requisition requisition,
      Map<VersionIdentityDto, OrderableDto> orderables, Profiler profiler) {
    profiler.start("BUILD_DTO");
    RequisitionV2Dto dto = new RequisitionV2Dto();
    requisition.export(dto);

    dto.setTemplate(BasicRequisitionTemplateDto.newInstance(requisition.getTemplate()));
    dto.setFacility(new ObjectReferenceDto(requisition.getFacilityId(), serviceUrl, FACILITIES));
    dto.setProcessingPeriod(new ObjectReferenceDto(requisition.getProcessingPeriodId(),
        serviceUrl, PROCESSING_PERIODS));
    dto.setProgram(new ObjectReferenceDto(requisition.getProgramId(), serviceUrl, PROGRAMS));

    List<RequisitionLineItem> requisitionLineItems = requisition.getRequisitionLineItems();
    List<RequisitionLineItemV2Dto> lineItems = requisitionLineItems
        .stream()
        .map(line -> {
          OrderableDto orderable = orderables
              .get(new VersionIdentityDto(line.getOrderable()));

          // The whole object is not required here
          ApprovedProductDto approvedProduct = new ApprovedProductDto(
              line.getFacilityTypeApprovedProduct().getId(), null, null, null,
              null, null, new MetadataDto(
                  line.getFacilityTypeApprovedProduct().getVersionId().toString(), null));

          RequisitionLineItemV2Dto lineDto = new RequisitionLineItemV2Dto();
          lineDto.setServiceUrl(serviceUrl);

          line.export(lineDto, orderable, approvedProduct);

          return lineDto;
        })
        .collect(Collectors.toList());

    dto.setRequisitionLineItems(lineItems);

    Set<VersionObjectReferenceDto> availableFullSupply = new HashSet<>();
    Set<VersionObjectReferenceDto> availableNonFullSupply = new HashSet<>();

    Optional
        .ofNullable(requisition.getAvailableProducts())
        .orElse(Collections.emptySet())
        .stream()
        .map(ApprovedProductReference::getOrderable)
        .map(identity -> orderables.get(new VersionIdentityDto(identity)))
        .forEach(orderable -> {
          ProgramOrderableDto po = orderable.getProgramOrderable(requisition.getProgramId());
          VersionObjectReferenceDto reference = new VersionObjectReferenceDto(
              orderable.getId(), serviceUrl, ORDERABLES, orderable.getVersionId());

          if (po.getFullSupply()) {
            availableFullSupply.add(reference);
          } else {
            availableNonFullSupply.add(reference);
          }
        });

    dto.setAvailableFullSupplyProducts(availableFullSupply);
    dto.setAvailableNonFullSupplyProducts(availableNonFullSupply);
    dto.setStockAdjustmentReasons(newInstance(requisition.getStockAdjustmentReasons()));
    return dto;
  }

}
