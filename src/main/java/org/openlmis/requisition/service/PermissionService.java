package org.openlmis.requisition.service;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_UPDATE_REQUISITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.ResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.web.PermissionMessageException;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@Service
public class PermissionService {
  private static final String REQUISITION_BASE = "REQUISITION_";

  static final String REQUISITION_CREATE = REQUISITION_BASE + "CREATE";
  static final String REQUISITION_APPROVE = REQUISITION_BASE + "APPROVE";
  static final String REQUISITION_AUTHORIZE = REQUISITION_BASE + "AUTHORIZE";
  static final String REQUISITION_DELETE = REQUISITION_BASE + "DELETE";
  static final String REQUISITION_VIEW = REQUISITION_BASE + "VIEW";
  static final String REQUISITION_CONVERT_TO_ORDER = REQUISITION_BASE + "CONVERT_TO_ORDER";
  static final String REQUISITION_TEMPLATES_MANAGE = "REQUISITION_TEMPLATES_MANAGE";

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  /**
   * Checks if current user has permission to initiate a requisition.
   *
   * @throws PermissionMessageException if the current user has not a permission.
   */
  public void canInitRequisition(UUID program, UUID facility) {
    checkPermission(REQUISITION_CREATE, program, facility, null);
  }

  /**
   * Checks if current user has permission to update a requisition.
   * Permissions needed to perform update action depend on the requisition status.
   *
   * @param requisitionId UUID of requisition.
   * @throws PermissionMessageException if the current user has not a permission.
   * @throws IllegalStateException      if requisition has incorrect status.
   */
  public void canUpdateRequisition(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition != null) {
      switch (requisition.getStatus()) {
        case INITIATED:
          checkPermission(REQUISITION_CREATE, requisitionId);
          break;
        case SUBMITTED:
          checkPermission(REQUISITION_AUTHORIZE, requisitionId);
          break;
        case AUTHORIZED:
          checkPermission(REQUISITION_APPROVE, requisitionId);
          break;
        default:
          throw new ValidationMessageException(ERROR_CANNOT_UPDATE_REQUISITION);
      }
    }
  }

  /**
   * Checks if current user has permission to submit a requisition.
   */
  public void canSubmitRequisition(UUID requisitionId) {
    checkPermission(REQUISITION_CREATE, requisitionId);
  }

  /**
   * Checks if current user has permission to approve a requisition.
   */
  public void canApproveRequisition(UUID requisitionId) {
    checkPermission(REQUISITION_APPROVE, requisitionId);
  }

  /**
   * Checks if current user has permission to authorize a requisition.
   */
  public void canAuthorizeRequisition(UUID requisitionId) {
    checkPermission(REQUISITION_AUTHORIZE, requisitionId);
  }

  /**
   * Checks if current user has permission to delete a requisition.
   */
  public void canDeleteRequisition(UUID requisitionId) {
    checkPermission(REQUISITION_DELETE, requisitionId);
  }

  /**
   * Checks if current user has permission to view a requisition.
   */
  public void canViewRequisition(UUID requisitionId) {
    checkPermission(REQUISITION_VIEW, requisitionId);
  }

  /**
   * Checks if current user has permission to convert requisition to order.
   *
   * @param list of ConvertToOrderDtos containing chosen requisitionId and supplyingDepotId.
   * @throws PermissionMessageException if the current user has not a permission.
   */
  public void canConvertToOrder(List<ConvertToOrderDto> list) {
    for (ConvertToOrderDto convertToOrder : list) {
      Requisition requisition = requisitionRepository.findOne(convertToOrder.getRequisitionId());
      if (requisition == null) {
        throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
            convertToOrder.getRequisitionId()));
      }
      checkPermission(REQUISITION_CONVERT_TO_ORDER, null, null,
          convertToOrder.getSupplyingDepotId());
    }
  }

  /**
   * Checks if current user has permission to manage a requisition template.
   */
  public void canManageRequisitionTemplate() {
    OAuth2Authentication authentication = (OAuth2Authentication) SecurityContextHolder.getContext()
        .getAuthentication();

    if (!authentication.isClientOnly()) {
      checkPermission(REQUISITION_TEMPLATES_MANAGE, null, null, null);
    }
  }

  private void checkPermission(String rightName, UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (null != requisition) {
      checkPermission(rightName, requisition.getProgramId(), requisition.getFacilityId(), null);
    }
  }

  private void checkPermission(String rightName, UUID program, UUID facility, UUID warehouse) {
    if (!hasPermission(rightName, program, facility, warehouse)) {
      throw new PermissionMessageException( new Message(ERROR_NO_FOLLOWING_PERMISSION, rightName));
    }
  }

  private Boolean hasPermission(String rightName, UUID program, UUID facility, UUID warehouse) {
    UserDto user = authenticationHelper.getCurrentUser();
    RightDto right = authenticationHelper.getRight(rightName);
    ResultDto<Boolean> result = userReferenceDataService.hasRight(
        user.getId(), right.getId(), program, facility, warehouse
    );
    return null != result && result.getResult();
  }


}
