package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.BooleanResultDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.web.MissingPermissionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.AuthenticationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
public class PermissionService {
  private static final String REQUISITION_BASE = "REQUISITION_";

  static final String REQUISITION_CREATE = REQUISITION_BASE + "CREATE";
  static final String REQUISITION_APPROVE = REQUISITION_BASE + "APPROVE";
  static final String REQUISITION_AUTHORIZE = REQUISITION_BASE + "AUTHORIZE";
  static final String REQUISITION_DELETE = REQUISITION_BASE + "DELETE";
  static final String REQUISITION_VIEW = REQUISITION_BASE + "VIEW";
  static final String REQUISITION_CONVERT_TO_ORDER = REQUISITION_BASE + "CONVERT_TO_ORDER";

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
  public void canInitRequisition(UUID program, UUID facility) throws MissingPermissionException {
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
  public void canUpdateRequisition(UUID requisitionId) throws MissingPermissionException {
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
  public void canSubmitRequisition(UUID requisitionId) throws MissingPermissionException {
    hasPermission(REQUISITION_CREATE, requisitionId);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canApproveRequisition(UUID requisitionId) throws MissingPermissionException {
    hasPermission(REQUISITION_APPROVE, requisitionId);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canAuthorizeRequisition(UUID requisitionId) throws MissingPermissionException {
    hasPermission(REQUISITION_AUTHORIZE, requisitionId);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canDeleteRequisition(UUID requisitionId) throws MissingPermissionException {
    hasPermission(REQUISITION_DELETE, requisitionId);
  }

  /**
   * Checks if current user has permission to view a requisition.
   *
   * @throws MissingPermissionException   if the current user has not a permission.
   */
  public void canViewRequisition(UUID requisitionId) throws MissingPermissionException {
    hasPermission(REQUISITION_VIEW, requisitionId);
  }

  /**
   * Chacks if current user has permission to convert requisition to order.
   * @param list of ConvertToOrderDtos containing chosen requisitionId and supplyingDepotId.
   * @throws MissingPermissionException if the current user has not a permission.
   */
  public void canConvertToOrder(List<ConvertToOrderDto> list) throws MissingPermissionException,
      RequisitionException {
    for (ConvertToOrderDto convertToOrder : list) {
      Requisition requisition = requisitionRepository.findOne(convertToOrder.getRequisitionId());
      if (requisition == null) {
        throw new RequisitionNotFoundException(convertToOrder.getRequisitionId());
      }
      hasPermission(REQUISITION_CONVERT_TO_ORDER, requisition.getProgramId(),
          convertToOrder.getSupplyingDepotId());
    }
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
