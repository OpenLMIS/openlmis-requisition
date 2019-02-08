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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_UPDATE_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION_FOR_REQUISITION_UPDATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ReleasableRequisitionDto;
import org.openlmis.requisition.dto.ResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.PermissionStrings;
import org.openlmis.requisition.service.referencedata.RoleReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.stereotype.Service;

@SuppressWarnings("PMD.TooManyMethods")
@Service
public class PermissionService {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(PermissionService.class);

  private static final String REQUISITION_BASE = "REQUISITION_";

  public static final String REQUISITION_CREATE = REQUISITION_BASE + "CREATE";
  public static final String REQUISITION_APPROVE = REQUISITION_BASE + "APPROVE";
  public static final String REQUISITION_AUTHORIZE = REQUISITION_BASE + "AUTHORIZE";
  public static final String REQUISITION_DELETE = REQUISITION_BASE + "DELETE";
  public static final String REQUISITION_VIEW = REQUISITION_BASE + "VIEW";
  static final String REQUISITION_TEMPLATES_MANAGE = REQUISITION_BASE + "TEMPLATES_MANAGE";

  private static final String REPORT_TEMPLATES_EDIT = "REPORT_TEMPLATES_EDIT";
  private static final String REPORTS_VIEW = "REPORTS_VIEW";

  public static final String ORDERS_EDIT = "ORDERS_EDIT";

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private RoleReferenceDataService roleReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private PermissionStrings permissionStrings;

  @Value("${auth.server.clientId}")
  private String serviceTokenClientId;

  /**
   * Checks if current user has permission to initiate a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canInitRequisition(UUID program, UUID facility) {
    return checkSupervisionPermission(REQUISITION_CREATE, facility, program);
  }

  /**
   * Checks if current user has permission to initiate or authorize a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canInitOrAuthorizeRequisition(UUID program, UUID facility) {
    ValidationResult result = checkSupervisionPermission(REQUISITION_CREATE, facility, program);

    if (result.isSuccess()) {
      return result;
    }

    result = checkSupervisionPermission(REQUISITION_AUTHORIZE, facility, program);

    if (result.isSuccess()) {
      return result;
    }

    return ValidationResult.noPermission(
        ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_CREATE, REQUISITION_AUTHORIZE);
  }

  /**
   * Checks if current user has permission to update a requisition. Permissions needed to perform
   * update action depend on the requisition status.
   *
   * @param requisition the requisition.
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canUpdateRequisition(Requisition requisition) {
    if (requisition != null) {
      switch (requisition.getStatus()) {
        case INITIATED:
        case REJECTED:
          return checkPermissionOnUpdate(REQUISITION_CREATE, requisition);
        case SUBMITTED:
          return checkPermissionOnUpdate(REQUISITION_AUTHORIZE, requisition);
        case AUTHORIZED:
        case IN_APPROVAL:
          return checkPermissionOnUpdate(REQUISITION_APPROVE, requisition);
        default:
          return ValidationResult.failedValidation(ERROR_CANNOT_UPDATE_REQUISITION);
      }
    }

    return ValidationResult.success();
  }

  /**
   * Checks if current user has permission to submit a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canSubmitRequisition(Requisition requisition) {
    return checkSupervisionPermission(REQUISITION_CREATE, requisition);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canApproveRequisition(Requisition requisition) {
    return checkSupervisionPermission(REQUISITION_APPROVE, requisition);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canAuthorizeRequisition(Requisition requisition) {
    return checkSupervisionPermission(REQUISITION_AUTHORIZE, requisition);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canDeleteRequisition(Requisition requisition) {
    ValidationResult result = checkSupervisionPermission(REQUISITION_DELETE, requisition);

    if (result.hasErrors()) {
      return result;
    }

    if (requisition.getStatus().isSubmittable() || requisition.getStatus().isSkipped()) {
      return checkSupervisionPermission(REQUISITION_CREATE, requisition);
    }

    if (requisition.getStatus().equals(RequisitionStatus.SUBMITTED)) {
      return checkSupervisionPermission(REQUISITION_AUTHORIZE, requisition);
    }

    return ValidationResult.success();
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewRequisition(UUID requisitionId) {
    return canViewRequisition(requisitionRepository.findOne(requisitionId));
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewRequisition(Requisition requisition) {
    return checkSupervisionPermission(REQUISITION_VIEW, requisition);
  }

  /**
   * Checks if current user has permission to convert requisition to order.
   *
   * @param list of ReleasableRequisitionDto containing requisitionId and supplyingDepotId.
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canConvertToOrder(List<ReleasableRequisitionDto> list) {
    Set<UUID> requisitionIds = list
        .stream()
        .map(ReleasableRequisitionDto::getRequisitionId)
        .collect(Collectors.toSet());

    Map<UUID, Requisition> requisitions = requisitionRepository
        .findAll(requisitionIds)
        .stream()
        .collect(Collectors.toMap(BaseEntity::getId, Function.identity()));

    for (ReleasableRequisitionDto convertToOrder : list) {
      Requisition requisition = requisitions.get(convertToOrder.getRequisitionId());

      if (requisition == null) {
        return ValidationResult
            .notFound(ERROR_REQUISITION_NOT_FOUND, convertToOrder.getRequisitionId());
      }

      ValidationResult validation = checkFulfillmentPermission(ORDERS_EDIT,
          convertToOrder.getSupplyingDepotId());

      if (validation.hasErrors()) {
        return validation;
      }
    }

    return ValidationResult.success();
  }

  /**
   * Checks if current user has permission to manage a requisition template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canManageRequisitionTemplate() {
    return checkGeneralPermission(REQUISITION_TEMPLATES_MANAGE);
  }

  /**
   * Checks if current user has permission to edit a report template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canEditReportTemplates() {
    return checkGeneralPermission(REPORT_TEMPLATES_EDIT);
  }

  /**
   * Checks if current user has permission to view a report template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewReports() {
    return checkGeneralPermission(REPORTS_VIEW);
  }

  private ValidationResult checkPermissionOnUpdate(String rightName, Requisition requisition) {
    ValidationResult result = checkSupervisionPermission(rightName, requisition);

    if (result.isSuccess()) {
      return result;
    }

    RequisitionStatus status = requisition.getStatus();

    if (status.duringApproval()) {
      status = RequisitionStatus.AUTHORIZED;
    }

    return ValidationResult
        .noPermission(ERROR_NO_FOLLOWING_PERMISSION_FOR_REQUISITION_UPDATE,
            status.toString(), rightName);
  }

  private ValidationResult checkGeneralPermission(String rightName) {
    return validateRequest(new DefaultRequestDetails(rightName));
  }

  private ValidationResult checkSupervisionPermission(String rightName, Requisition requisition) {
    return validateRequest(new RequisitionRequestDetails(rightName, requisition));
  }

  private ValidationResult checkSupervisionPermission(String rightName,
      UUID facility, UUID program) {
    return validateRequest(new DefaultRequestDetails(rightName, facility, program));
  }

  private ValidationResult checkFulfillmentPermission(String rightName, UUID warehouse) {
    return validateRequest(new DefaultRequestDetails(rightName, warehouse));
  }

  private ValidationResult validateRequest(RequestDetails details) {
    XLOGGER.entry(details);
    Profiler profiler = new Profiler("CHECK_PERMISSION");
    profiler.setLogger(XLOGGER);

    profiler.start("GET_AUTHENTICATION");
    OAuth2Authentication authentication = (OAuth2Authentication) SecurityContextHolder
        .getContext()
        .getAuthentication();

    boolean hasPermission = authentication.isClientOnly()
        ? checkServiceToken(authentication)
        : checkUserToken(details, profiler);

    ValidationResult result = hasPermission
        ? ValidationResult.success()
        : ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, details.getRightName());

    profiler.stop().log();
    XLOGGER.exit(result);

    return result;
  }

  private boolean checkUserToken(RequestDetails details, Profiler profiler) {
    profiler.start("GET_CURRENT_USER");
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("GET_RIGHT");
    RightDto right = authenticationHelper.getRight(details.getRightName());

    if (REQUISITION_APPROVE.equalsIgnoreCase(details.getRightName())) {
      return checkUserTokenForApprove(user, right, details, profiler);
    } else {
      profiler.start("CHECK_HAS_RIGHT");
      ResultDto<Boolean> result = userReferenceDataService.hasRight(
          user.getId(), right.getId(), details.getProgramId(),
          details.getFacilityId(), details.getWarehouseId());

      return null != result && result.getResult();
    }
  }

  private boolean checkUserTokenForApprove(UserDto user, RightDto right,
      RequestDetails details, Profiler profiler) {
    // we can't be sure that a user will have correct rights based on permission strings
    // that is why we need to verfiy if the user has a correct right by checking user's
    // roles

    profiler.start("GET_ROLES_FOR_RIGHT");
    List<RoleDto> roles = roleReferenceDataService.search(right.getId());

    profiler.start("CHECK_HAS_ROLE");
    for (int i = 0, length = roles.size(); i < length; ++i) {
      RoleDto role = roles.get(i);

      if (user.hasSupervisorySupervisionRole(role.getId(),
          details.getProgramId(), details.getSupervisoryNodeId())) {
        return true;
      }

      if (!details.containsPartnerRequisition()
          && user.hasHomeFacilitySupervisionRole(role.getId(),
          details.getProgramId(), details.getFacilityId())) {
        return true;
      }
    }

    return false;
  }

  private boolean checkServiceToken(OAuth2Authentication authentication) {
    String clientId = authentication.getOAuth2Request().getClientId();

    // we accept only service's requests
    return serviceTokenClientId.equals(clientId);
  }

  /**
   * Get current user's permission strings.
   *
   * @return user's permission strings
   */
  public PermissionStrings.Handler getPermissionStrings(UUID userId) {
    return permissionStrings.forUser(userId);
  }

  private interface RequestDetails {

    String getRightName();

    UUID getFacilityId();

    UUID getProgramId();

    UUID getWarehouseId();

    default UUID getSupervisoryNodeId() {
      return null;
    }

    default boolean containsPartnerRequisition() {
      return false;
    }

  }

  @Getter
  @AllArgsConstructor
  @ToString
  private static class DefaultRequestDetails implements RequestDetails {

    private String rightName;

    private UUID facilityId;
    private UUID programId;

    private UUID warehouseId;

    DefaultRequestDetails(String rightName) {
      this(rightName, null, null, null);
    }

    DefaultRequestDetails(String rightName, UUID facilityId, UUID programId) {
      this(rightName, facilityId, programId, null);
    }

    DefaultRequestDetails(String rightName, UUID warehouseId) {
      this(rightName, null, null, warehouseId);
    }

  }

  @ToString
  private static final class RequisitionRequestDetails extends DefaultRequestDetails {

    private Requisition requisition;

    RequisitionRequestDetails(String rightName, Requisition requisition) {
      super(rightName, requisition.getFacilityId(), requisition.getProgramId());
      this.requisition = requisition;
    }

    @Override
    public UUID getSupervisoryNodeId() {
      return requisition.getSupervisoryNodeId();
    }

    @Override
    public boolean containsPartnerRequisition() {
      return requisition.hasOriginalRequisitionId();
    }

  }
}
