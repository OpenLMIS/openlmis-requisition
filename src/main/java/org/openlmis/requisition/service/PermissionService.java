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

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.ResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.stereotype.Service;

@SuppressWarnings("PMD.TooManyMethods")
@Service
public class PermissionService {
  private static final String REQUISITION_BASE = "REQUISITION_";

  public static final String REQUISITION_CREATE = REQUISITION_BASE + "CREATE";
  public static final String REQUISITION_APPROVE = REQUISITION_BASE + "APPROVE";
  public static final String REQUISITION_AUTHORIZE = REQUISITION_BASE + "AUTHORIZE";
  public static final String REQUISITION_DELETE = REQUISITION_BASE + "DELETE";
  public static final String REQUISITION_VIEW = REQUISITION_BASE + "VIEW";
  public static final String REQUISITION_TEMPLATES_MANAGE = "REQUISITION_TEMPLATES_MANAGE";
  public static final String REPORT_TEMPLATES_EDIT = "REPORT_TEMPLATES_EDIT";
  public static final String REPORTS_VIEW = "REPORTS_VIEW";

  static final String ORDERS_EDIT = "ORDERS_EDIT";

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  /**
   * Checks if current user has permission to initiate a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canInitRequisition(UUID program, UUID facility) {
    return checkPermission(REQUISITION_CREATE, program, facility, null);
  }

  /**
   * Checks if current user has permission to initiate or authorize a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canInitOrAuthorizeRequisition(UUID program, UUID facility) {
    if (!hasPermission(REQUISITION_CREATE, program, facility, null)
        && !hasPermission(REQUISITION_AUTHORIZE, program, facility, null)) {
      return ValidationResult.noPermission(
          ERROR_NO_FOLLOWING_PERMISSION, REQUISITION_CREATE, REQUISITION_AUTHORIZE);
    }
    return ValidationResult.success();
  }

  /**
   * Checks if current user has permission to update a requisition.
   * Permissions needed to perform update action depend on the requisition status.
   *
   * @param requisitionId UUID of requisition.
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canUpdateRequisition(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

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
  public ValidationResult canSubmitRequisition(UUID requisitionId) {
    return checkPermission(REQUISITION_CREATE, requisitionId);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canApproveRequisition(UUID requisitionId) {
    return checkPermission(REQUISITION_APPROVE, requisitionId);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canAuthorizeRequisition(UUID requisitionId) {
    return checkPermission(REQUISITION_AUTHORIZE, requisitionId);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canDeleteRequisition(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition != null) {
      ValidationResult permissionCheck = checkPermission(REQUISITION_DELETE, requisition);
      if (permissionCheck.hasErrors()) {
        return permissionCheck;
      }
      if (requisition.getStatus().isSubmittable()) {
        return checkPermission(REQUISITION_CREATE, requisition);
      } else if (requisition.getStatus().equals(RequisitionStatus.SUBMITTED)) {
        return checkPermission(REQUISITION_AUTHORIZE, requisition);
      }
    } else {
      return ValidationResult.notFound(ERROR_REQUISITION_NOT_FOUND, requisitionId);
    }
    return ValidationResult.success();
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewRequisition(UUID requisitionId) {
    return checkPermission(REQUISITION_VIEW, requisitionId);
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewRequisition(Requisition requisition) {
    return checkPermission(REQUISITION_VIEW, requisition.getProgramId(),
        requisition.getFacilityId(), null);
  }

  /**
   * Checks if current user has permission to convert requisition to order.
   *
   * @param list of ConvertToOrderDtos containing chosen requisitionId and supplyingDepotId.
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canConvertToOrder(List<ConvertToOrderDto> list) {
    for (ConvertToOrderDto convertToOrder : list) {
      Requisition requisition = requisitionRepository.findOne(convertToOrder.getRequisitionId());
      if (requisition == null) {
        return ValidationResult.notFound(
            ERROR_REQUISITION_NOT_FOUND, convertToOrder.getRequisitionId());
      }
      ValidationResult validation = checkPermission(ORDERS_EDIT, null, null,
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
    return checkPermission(REQUISITION_TEMPLATES_MANAGE, null, null, null);
  }

  /**
   * Checks if current user has permission to edit a report template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canEditReportTemplates() {
    return checkPermission(REPORT_TEMPLATES_EDIT, null, null, null);
  }

  /**
   * Checks if current user has permission to view a report template.
   *
   * @return ValidationResult containing info about the result of this check
   */
  public ValidationResult canViewReports() {
    return checkPermission(REPORTS_VIEW, null, null, null);
  }

  private ValidationResult checkPermissionOnUpdate(String rightName, Requisition requisition) {
    if (!hasPermission(rightName, requisition.getProgramId(), requisition.getFacilityId(), null)) {
      RequisitionStatus status = requisition.getStatus();
      if (status.duringApproval()) {
        status = RequisitionStatus.AUTHORIZED;
      }
      return ValidationResult.noPermission(
          ERROR_NO_FOLLOWING_PERMISSION_FOR_REQUISITION_UPDATE, status.toString(), rightName);
    }
    return ValidationResult.success();
  }

  private ValidationResult checkPermission(String rightName, UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (null != requisition) {
      return checkPermission(rightName, requisition);
    } else {
      return ValidationResult.notFound(ERROR_REQUISITION_NOT_FOUND, requisitionId);
    }
  }

  private ValidationResult checkPermission(String rightName, Requisition requisition) {
    return checkPermission(
        rightName, requisition.getProgramId(), requisition.getFacilityId(), null);
  }

  private ValidationResult checkPermission(String rightName, UUID program, UUID facility,
                                           UUID warehouse) {
    if (!hasPermission(rightName, program, facility, warehouse)) {
      return ValidationResult.noPermission(ERROR_NO_FOLLOWING_PERMISSION, rightName);
    }
    return ValidationResult.success();
  }

  private Boolean hasPermission(String rightName, UUID program, UUID facility, UUID warehouse) {
    OAuth2Authentication authentication = (OAuth2Authentication) SecurityContextHolder.getContext()
            .getAuthentication();
    if (authentication.isClientOnly()) {
      return true;
    }
    UserDto user = authenticationHelper.getCurrentUser();
    RightDto right = authenticationHelper.getRight(rightName);
    ResultDto<Boolean> result = userReferenceDataService.hasRight(
        user.getId(), right.getId(), program, facility, warehouse
    );
    return null != result && result.getResult();
  }

  /**
   * Get current user's permission strings.
   * @return user's permission strings
   */
  public List<String> getPermissionStrings() {
    OAuth2Authentication authentication = (OAuth2Authentication) SecurityContextHolder.getContext()
        .getAuthentication();
    if (authentication.isClientOnly()) {
      return Collections.emptyList();
    }
    UserDto user = authenticationHelper.getCurrentUser();
    return userReferenceDataService.getPermissionStrings(user.getId());
  }
}
