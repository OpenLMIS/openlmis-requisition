package org.openlmis.utils;

import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.BooleanResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class PermissionHelper {
  private static final String REQUISITION_BASE = "REQUISITION_";
  private static final String REQUISITION_CREATE = REQUISITION_BASE + "CREATE";
  private static final String REQUISITION_APPROVE = REQUISITION_BASE + "APPROVE";
  private static final String REQUISITION_AUTHORIZE = REQUISITION_BASE + "AUTHORIZE";
  private static final String REQUISITION_DELETE = REQUISITION_BASE + "DELETE";
  private static final String REQUISITION_VIEW = REQUISITION_BASE + "VIEW";

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  /**
   * Checks if current user has permission to initiate a requisition.
   *
   * @return true if current user has permission; otherwise false.
   */
  public boolean canInitRequisition(UUID program, UUID facility) {
    return hasPermission(REQUISITION_CREATE, program, facility);
  }

  /**
   * Checks if current user has permission to update a requisition.
   * Permissions needed to perform update action depend on the requisition status.
   *
   * @param requisitionStatus status of a requisition to update.
   * @return true if current user has permission; otherwise false.
   */
  public boolean canUpdateRequisition(RequisitionStatus requisitionStatus) {
    return requisitionStatus == RequisitionStatus.INITIATED && hasPermission(REQUISITION_CREATE)
        || requisitionStatus == RequisitionStatus.SUBMITTED && hasPermission(REQUISITION_AUTHORIZE)
        || requisitionStatus == RequisitionStatus.AUTHORIZED && hasPermission(REQUISITION_APPROVE);
  }

  /**
   * Checks if current user has permission to submit a requisition.
   *
   * @return true if current user has permission; otherwise false.
   */
  public boolean canSubmitRequisition() {
    return hasPermission(REQUISITION_CREATE);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   *
   * @return true if current user has permission; otherwise false.
   */
  public boolean canApproveRequisition() {
    return hasPermission(REQUISITION_APPROVE);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   *
   * @return true if current user has permission; otherwise false.
   */
  public boolean canAuthorizeRequisition() {
    return hasPermission(REQUISITION_AUTHORIZE);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   *
   * @return true if current user has permission; otherwise false.
   */
  public boolean canDeleteRequisition() {
    return hasPermission(REQUISITION_DELETE);
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @return true if current user has permission; otherwise false.
   */
  public boolean canViewRequisition() {
    return hasPermission(REQUISITION_VIEW);
  }

  private boolean hasPermission(String rightName) {
    return hasPermission(rightName, null, null);
  }

  private boolean hasPermission(String rightName, UUID program, UUID facility) {
    UserDto user = authenticationHelper.getCurrentUser();
    RightDto right = authenticationHelper.getRight(rightName);
    BooleanResultDto result = userReferenceDataService.hasRight(
        user.getId(), right.getId(), program, facility
    );

    return null != result && result.isResult();
  }

}
