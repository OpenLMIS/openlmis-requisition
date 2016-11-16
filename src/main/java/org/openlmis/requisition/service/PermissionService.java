package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.BooleanResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.web.MissingPermissionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.AuthenticationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class PermissionService {
  private static final String REQUISITION_BASE = "REQUISITION_";

  static final String REQUISITION_CREATE = REQUISITION_BASE + "CREATE";
  static final String REQUISITION_APPROVE = REQUISITION_BASE + "APPROVE";
  static final String REQUISITION_AUTHORIZE = REQUISITION_BASE + "AUTHORIZE";
  static final String REQUISITION_DELETE = REQUISITION_BASE + "DELETE";
  static final String REQUISITION_VIEW = REQUISITION_BASE + "VIEW";

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  /**
   * Checks if current user has permission to initiate a requisition.
   *
   * @throws MissingPermissionException if the current user has not a permission.
   */
  public void canInitRequisition(UUID program, UUID facility) {
    hasPermission(REQUISITION_VIEW, program, facility);
    hasPermission(REQUISITION_CREATE, program, facility);
  }

  /**
   * Checks if current user has permission to update a requisition.
   * Permissions needed to perform update action depend on the requisition status.
   *
   * @param requisitionId UUID of requisition.
   * @throws MissingPermissionException   if the current user has not a permission.
   * @throws IllegalStateException        if requisition has incorrect status.
   */
  public void canUpdateRequisition(UUID requisitionId) {
    canViewRequisition(requisitionId);

    Requisition requisition = requisitionRepository.findOne(requisitionId);

    switch (requisition.getStatus()) {
      case INITIATED:
        hasPermission(REQUISITION_CREATE, requisitionId);
        break;
      case SUBMITTED:
        hasPermission(REQUISITION_AUTHORIZE, requisitionId);
        break;
      case AUTHORIZED:
        hasPermission(REQUISITION_APPROVE, requisitionId);
        break;
      default:
        throw new IllegalStateException("Requisition has incorrect status");
    }
  }

  /**
   * Checks if current user has permission to submit a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canSubmitRequisition(UUID requisitionId) {
    canViewRequisition(requisitionId);
    hasPermission(REQUISITION_CREATE, requisitionId);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canApproveRequisition(UUID requisitionId) {
    canViewRequisition(requisitionId);
    hasPermission(REQUISITION_APPROVE, requisitionId);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canAuthorizeRequisition(UUID requisitionId) {
    canViewRequisition(requisitionId);
    hasPermission(REQUISITION_AUTHORIZE, requisitionId);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canDeleteRequisition(UUID requisitionId) {
    canViewRequisition(requisitionId);
    hasPermission(REQUISITION_DELETE, requisitionId);
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canViewRequisition(UUID requisitionId) {
    hasPermission(REQUISITION_VIEW, requisitionId);
  }

  private void hasPermission(String rightName, UUID requisitionId)
      throws MissingPermissionException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (null != requisition) {
      hasPermission(rightName, requisition.getProgramId(), requisition.getFacilityId());
    }
  }

  private void hasPermission(String rightName, UUID program, UUID facility)
      throws MissingPermissionException {
    UserDto user = authenticationHelper.getCurrentUser();
    RightDto right = authenticationHelper.getRight(rightName);
    BooleanResultDto result = userReferenceDataService.hasRight(
        user.getId(), right.getId(), program, facility
    );

    if (null == result || !result.isResult()) {
      throw new MissingPermissionException(rightName);
    }
  }

}
