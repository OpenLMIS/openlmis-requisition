package org.openlmis.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.BooleanResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;

import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class PermissionHelperTest {

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @InjectMocks
  private PermissionHelper permissionHelper;

  @Before
  public void setUp() {
    UserDto userMock = mock(UserDto.class);
    RightDto rightDto = mock(RightDto.class);
    when(authenticationHelper.getCurrentUser()).thenReturn(userMock);
    when(authenticationHelper.getRight(anyString())).thenReturn(rightDto);
  }

  @Test
  public void canInitRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canInitRequisition(UUID.randomUUID(), UUID.randomUUID());

    //then
    assertTrue(result);
  }

  @Test
  public void cannotInitRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canInitRequisition(UUID.randomUUID(), UUID.randomUUID());

    //then
    assertFalse(result);
  }

  @Test
  public void canUpdateRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canUpdateRequisition(RequisitionStatus.INITIATED);

    //then
    assertTrue(result);
  }

  @Test
  public void cannotUpdateRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canUpdateRequisition(RequisitionStatus.INITIATED);

    //then
    assertFalse(result);
  }

  @Test
  public void canSubmitRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canSubmitRequisition();

    //then
    assertTrue(result);
  }

  @Test
  public void cannotSubmitRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canSubmitRequisition();

    //then
    assertFalse(result);
  }


  @Test
  public void canApproveRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canApproveRequisition();

    //then
    assertTrue(result);
  }

  @Test
  public void cannotApproveRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canApproveRequisition();

    //then
    assertFalse(result);
  }


  @Test
  public void canAuthorizeRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canAuthorizeRequisition();

    //then
    assertTrue(result);
  }

  @Test
  public void cannotAuthorizeRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canAuthorizeRequisition();

    //then
    assertFalse(result);
  }


  @Test
  public void canDeleteRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canDeleteRequisition();

    //then
    assertTrue(result);
  }

  @Test
  public void cannotDeleteRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canDeleteRequisition();

    //then
    assertFalse(result);
  }


  @Test
  public void canViewRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(true);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canViewRequisition();

    //then
    assertTrue(result);
  }

  @Test
  public void cannotViewRequisition() {
    //given
    BooleanResultDto resultDto = new BooleanResultDto(false);
    when(userReferenceDataService.hasRight(any(), any(), any(), any())).thenReturn(resultDto);

    //when
    boolean result = permissionHelper.canViewRequisition();

    //then
    assertFalse(result);
  }

}
