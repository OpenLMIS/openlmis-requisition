package org.openlmis.requisition.service;

import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_DELETE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.BooleanResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.web.MissingPermissionException;
import org.openlmis.utils.AuthenticationHelper;

import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class PermissionServiceTest {

  @Rule
  public final ExpectedException exception = ExpectedException.none();

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private RequisitionRepository requisitionRepository;

  @InjectMocks
  private PermissionService permissionService;

  @Mock
  private UserDto user;

  @Mock
  private RightDto requisitionCreateRight;

  @Mock
  private RightDto requisitionApproveRight;

  @Mock
  private RightDto requisitionAuthorizeRight;

  @Mock
  private RightDto requisitionDeleteRight;

  @Mock
  private RightDto requisitionViewRight;

  @Mock
  private Requisition requisition;

  private UUID userId = UUID.randomUUID();
  private UUID requisitionCreateRightId = UUID.randomUUID();
  private UUID requisitionApproveRightId = UUID.randomUUID();
  private UUID requisitionAuthorizeRightId = UUID.randomUUID();
  private UUID requisitionDeleteRightId = UUID.randomUUID();
  private UUID requisitionViewRightId = UUID.randomUUID();
  private UUID requisitionId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();

  @Before
  public void setUp() {
    when(user.getId()).thenReturn(userId);

    when(requisitionCreateRight.getId()).thenReturn(requisitionCreateRightId);
    when(requisitionApproveRight.getId()).thenReturn(requisitionApproveRightId);
    when(requisitionAuthorizeRight.getId()).thenReturn(requisitionAuthorizeRightId);
    when(requisitionDeleteRight.getId()).thenReturn(requisitionDeleteRightId);
    when(requisitionViewRight.getId()).thenReturn(requisitionViewRightId);

    when(requisition.getId()).thenReturn(requisitionId);
    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getFacilityId()).thenReturn(facilityId);

    when(authenticationHelper.getCurrentUser()).thenReturn(user);

    when(authenticationHelper.getRight(REQUISITION_CREATE)).thenReturn(requisitionCreateRight);
    when(authenticationHelper.getRight(REQUISITION_APPROVE)).thenReturn(requisitionApproveRight);
    when(authenticationHelper.getRight(REQUISITION_AUTHORIZE))
        .thenReturn(requisitionAuthorizeRight);
    when(authenticationHelper.getRight(REQUISITION_DELETE)).thenReturn(requisitionDeleteRight);
    when(authenticationHelper.getRight(REQUISITION_VIEW)).thenReturn(requisitionViewRight);

    when(requisitionRepository.findOne(requisitionId)).thenReturn(requisition);
  }

  @Test
  public void canInitRequisition() throws Exception {
    hasRight(requisitionCreateRightId, true);

    permissionService.canInitRequisition(programId, facilityId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_CREATE, requisitionCreateRightId);
  }

  @Test
  public void cannotInitRequisition() throws Exception {
    expectException(REQUISITION_CREATE);

    permissionService.canInitRequisition(programId, facilityId);
  }

  @Test
  public void canUpdateRequisition() throws Exception {
    hasRight(requisitionCreateRightId, true);

    when(requisition.getStatus()).thenReturn(RequisitionStatus.INITIATED);

    permissionService.canUpdateRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_CREATE, requisitionCreateRightId);
  }

  @Test
  public void cannotUpdateRequisition() throws Exception {
    expectException(REQUISITION_CREATE);

    when(requisition.getStatus()).thenReturn(RequisitionStatus.INITIATED);

    permissionService.canUpdateRequisition(requisitionId);
  }

  @Test
  public void canSubmitRequisition() throws Exception {
    hasRight(requisitionCreateRightId, true);

    permissionService.canSubmitRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_CREATE, requisitionCreateRightId);
  }

  @Test
  public void cannotSubmitRequisition() throws Exception {
    expectException(REQUISITION_CREATE);

    permissionService.canSubmitRequisition(requisitionId);
  }

  @Test
  public void canApproveRequisition() throws Exception {
    hasRight(requisitionApproveRightId, true);

    permissionService.canApproveRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_APPROVE, requisitionApproveRightId);
  }

  @Test
  public void cannotApproveRequisition() throws Exception {
    expectException(REQUISITION_APPROVE);

    permissionService.canApproveRequisition(requisitionId);
  }

  @Test
  public void canAuthorizeRequisition() throws Exception {
    hasRight(requisitionAuthorizeRightId, true);

    permissionService.canAuthorizeRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_AUTHORIZE, requisitionAuthorizeRightId);
  }

  @Test
  public void cannotAuthorizeRequisition() throws Exception {
    expectException(REQUISITION_AUTHORIZE);

    permissionService.canAuthorizeRequisition(requisitionId);
  }

  @Test
  public void canDeleteRequisition() throws Exception {
    hasRight(requisitionDeleteRightId, true);

    permissionService.canDeleteRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_DELETE, requisitionDeleteRightId);
  }

  @Test
  public void cannotDeleteRequisition() throws Exception {
    expectException(REQUISITION_DELETE);

    permissionService.canDeleteRequisition(requisitionId);
  }

  @Test
  public void canViewRequisition() throws Exception {
    hasRight(requisitionViewRightId, true);

    permissionService.canViewRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyRight(order, REQUISITION_VIEW, requisitionViewRightId);
  }

  @Test
  public void cannotViewRequisition() throws Exception {
    expectException(REQUISITION_VIEW);

    permissionService.canViewRequisition(requisitionId);
  }

  private void hasRight(UUID rightId, boolean assign) {
    BooleanResultDto resultDto = new BooleanResultDto(assign);
    when(userReferenceDataService
        .hasRight(userId, rightId, programId, facilityId)
    ).thenReturn(resultDto);
  }

  private void expectException(String rightName) {
    exception.expect(MissingPermissionException.class);
    exception.expectMessage(
        "You do not have the following permission to perform this action: " + rightName
    );
  }

  private void verifyRight(InOrder order, String rightName, UUID rightId) {
    order.verify(authenticationHelper).getCurrentUser();
    order.verify(authenticationHelper).getRight(rightName);
    order.verify(userReferenceDataService).hasRight(userId, rightId, programId, facilityId);
  }

}
