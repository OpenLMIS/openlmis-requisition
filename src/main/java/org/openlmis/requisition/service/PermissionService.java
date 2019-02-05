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

import static org.apache.commons.lang3.StringUtils.startsWith;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_UPDATE_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION_FOR_REQUISITION_UPDATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
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
  static final String REQUISITION_TEMPLATES_MANAGE = "REQUISITION_TEMPLATES_MANAGE";
  private static final String REPORT_TEMPLATES_EDIT = "REPORT_TEMPLATES_EDIT";
  private static final String REPORTS_VIEW = "REPORTS_VIEW";

  public static final String ORDERS_EDIT = "ORDERS_EDIT";

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private PermissionStrings permissionStrings;

  @Value("${auth.server.clientId}")
  private String serviceTokenClientId;

  @Value("${auth.server.clientId.apiKey.prefix}")
  private String apiKeyPrefix;

  /**
   * Checks if current user has permission to initiate a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canInitRequisition(UUID program, UUID facility) {
    return checkPermission(REQUISITION_CREATE, program, facility, null, null, null);
  }

  /**
   * Checks if current user has permission to initiate or authorize a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canInitOrAuthorizeRequisition(UUID program, UUID facility) {
    if (hasPermission(REQUISITION_CREATE, program, facility, null, null, null)) {
      return ValidationResult.success();
    }

    if (hasPermission(REQUISITION_AUTHORIZE, program, facility, null, null, null)) {
      return ValidationResult.success();
    }

    return ValidationResult
        .noPermission(ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_CREATE, REQUISITION_AUTHORIZE);
  }

  /**
   * Checks if current user has permission to update a requisition.
   * Permissions needed to perform update action depend on the requisition status.
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
    return checkPermission(REQUISITION_CREATE, requisition);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canApproveRequisition(Requisition requisition) {
    return checkPermission(REQUISITION_APPROVE, requisition);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canAuthorizeRequisition(Requisition requisition) {
    return checkPermission(REQUISITION_AUTHORIZE, requisition);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canDeleteRequisition(Requisition requisition) {
    ValidationResult result = checkPermission(REQUISITION_DELETE, requisition);

    if (result.hasErrors()) {
      return result;
    }

    if (requisition.getStatus().isSubmittable() || requisition.getStatus().isSkipped()) {
      return checkPermission(REQUISITION_CREATE, requisition);
    } else if (requisition.getStatus().equals(RequisitionStatus.SUBMITTED)) {
      return checkPermission(REQUISITION_AUTHORIZE, requisition);
    }

    return ValidationResult.success();
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewRequisition(UUID requisitionId) {
    return checkPermission(REQUISITION_VIEW, requisitionRepository.findOne(requisitionId));
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewRequisition(Requisition requisition) {
    return checkPermission(REQUISITION_VIEW, requisition);
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

      ValidationResult validation = checkPermission(
          ORDERS_EDIT, null, null, convertToOrder.getSupplyingDepotId(), null, null);

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
    return checkPermission(REQUISITION_TEMPLATES_MANAGE, null, null, null, null, null);
  }

  /**
   * Checks if current user has permission to edit a report template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canEditReportTemplates() {
    return checkPermission(REPORT_TEMPLATES_EDIT, null, null, null, null, null);
  }

  /**
   * Checks if current user has permission to view a report template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewReports() {
    return checkPermission(REPORTS_VIEW, null, null, null, null, null);
  }

  private ValidationResult checkPermissionOnUpdate(String rightName, Requisition requisition) {
    ValidationResult result = checkPermission(rightName, requisition);

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

  private ValidationResult checkPermission(String rightName, Requisition requisition) {
    return checkPermission(rightName, requisition.getProgramId(),
        requisition.getFacilityId(), null, requisition.getSupervisoryNodeId(),
        requisition);
  }

  private ValidationResult checkPermission(String rightName, UUID program, UUID facility,
      UUID warehouse, UUID supervisoryNode, Requisition requisition) {
    if (hasPermission(rightName, program, facility, warehouse, supervisoryNode, requisition)) {
      return ValidationResult.success();
    }

    return ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, rightName);
  }

  private boolean hasPermission(String rightName, UUID program, UUID facility, UUID warehouse,
      UUID supervisoryNode, Requisition requisition) {
    XLOGGER.entry(rightName, program, facility, warehouse, supervisoryNode);
    Profiler profiler = new Profiler("HAS_PERMISSION");
    profiler.setLogger(XLOGGER);

    profiler.start("GET_AUTHENTICATION");
    OAuth2Authentication authentication = (OAuth2Authentication) SecurityContextHolder
        .getContext()
        .getAuthentication();

    boolean result = authentication.isClientOnly()
        ? checkServiceToken(authentication)
        : checkUserToken(rightName, program, facility, warehouse,
            supervisoryNode, requisition, profiler);

    profiler.stop().log();
    XLOGGER.exit(result);

    return result;
  }

  private boolean checkUserToken(String rightName, UUID program, UUID facility, UUID warehouse,
      UUID supervisoryNode, Requisition requisition, Profiler profiler) {
    profiler.start("GET_CURRENT_USER");
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("GET_RIGHT");
    RightDto right = authenticationHelper.getRight(rightName);

    if (REQUISITION_APPROVE.equalsIgnoreCase(rightName)) {
      return checkUserTokenForApprove(user, right, program, facility,
          supervisoryNode, requisition, profiler);
    } else {
      profiler.start("CHECK_HAS_RIGHT");
      ResultDto<Boolean> result = userReferenceDataService.hasRight(
          user.getId(), right.getId(), program, facility, warehouse
      );

      return null != result && result.getResult();
    }
  }

  private boolean checkUserTokenForApprove(UserDto user, RightDto right, UUID program,
      UUID facility, UUID supervisoryNode, Requisition requisition, Profiler profiler) {
    // we can't be sure that a user will have correct rights based on permission strings
    // that is why we need to verfiy if the user has a correct right by checking user's
    // roles

    profiler.start("GET_ROLES_FOR_RIGHT");
    List<RoleDto> roles = authenticationHelper.getRoles(right.getId());

    profiler.start("CHECK_HAS_ROLE");
    for (int i = 0, length = roles.size(); i < length; ++i) {
      RoleDto role = roles.get(i);

      if (user.hasSupervisorySupervisionRole(role.getId(), program, supervisoryNode)) {
        return true;
      }

      if (!requisition.hasOriginalRequisitionId()
          && user.hasHomeFacilitySupervisionRole(role.getId(), program, facility)) {
        return true;
      }
    }

    return false;
  }

  private boolean checkServiceToken(OAuth2Authentication authentication) {
    String clientId = authentication.getOAuth2Request().getClientId();

    if (serviceTokenClientId.equals(clientId)) {
      // we accept any service's requests
      return true;
    }

    if (startsWith(clientId, apiKeyPrefix)) {
      // we decline any API key's requests
      return false;
    }

    return false;
  }

  /**
   * Get current user's permission strings.
   * @return user's permission strings
   */
  public List<String> getPermissionStrings() {
    OAuth2Authentication authentication = (OAuth2Authentication) SecurityContextHolder
        .getContext()
        .getAuthentication();

    if (authentication.isClientOnly()) {
      return Collections.emptyList();
    }

    UserDto user = authenticationHelper.getCurrentUser();
    return userReferenceDataService.getPermissionStrings(user.getId());
  }

  public PermissionStrings.Handler getPermissionStrings(UUID userId) {
    return permissionStrings.forUser(userId);
  }
}
