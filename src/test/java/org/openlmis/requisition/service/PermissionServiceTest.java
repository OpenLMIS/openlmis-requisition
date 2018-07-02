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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_FOLLOWING_PERMISSION_FOR_REQUISITION_UPDATE;
import static org.openlmis.requisition.service.OAuth2AuthenticationDataBuilder.API_KEY_PREFIX;
import static org.openlmis.requisition.service.OAuth2AuthenticationDataBuilder.SERVICE_CLIENT_ID;
import static org.openlmis.requisition.service.OAuth2AuthenticationDataBuilder.asApiKey;
import static org.openlmis.requisition.service.OAuth2AuthenticationDataBuilder.asClient;
import static org.openlmis.requisition.service.OAuth2AuthenticationDataBuilder.asService;
import static org.openlmis.requisition.service.PermissionService.ORDERS_EDIT;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_AUTHORIZE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_CREATE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_DELETE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_TEMPLATES_MANAGE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ReleasableRequisitionDto;
import org.openlmis.requisition.dto.ResultDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.FailureType;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Message;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class PermissionServiceTest {

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private RequisitionRepository requisitionRepository;

  @InjectMocks
  private PermissionService permissionService;

  @Mock
  private Requisition requisition;

  private SecurityContext securityContext;

  private UserDto user = DtoGenerator.of(UserDto.class);
  private RightDto requisitionCreateRight = DtoGenerator.of(RightDto.class, 7).get(0);
  private RightDto requisitionApproveRight = DtoGenerator.of(RightDto.class, 7).get(1);
  private RightDto requisitionAuthorizeRight = DtoGenerator.of(RightDto.class, 7).get(2);
  private RightDto requisitionDeleteRight = DtoGenerator.of(RightDto.class, 7).get(3);
  private RightDto requisitionViewRight = DtoGenerator.of(RightDto.class, 7).get(4);
  private RightDto requisitionConvertRight = DtoGenerator.of(RightDto.class, 7).get(5);
  private RightDto manageRequisitionTemplateRight = DtoGenerator.of(RightDto.class, 7).get(6);

  private OAuth2Authentication trustedClient;
  private OAuth2Authentication userClient;
  private OAuth2Authentication apiKeyClient;

  private UUID requisitionId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private ReleasableRequisitionDto releasableRequisitionDto = new ReleasableRequisitionDto();
  private List<ReleasableRequisitionDto> releasableDtos = new ArrayList<>();

  @Before
  public void setUp() {
    securityContext = mock(SecurityContext.class);
    SecurityContextHolder.setContext(securityContext);

    trustedClient = asService();
    userClient = asClient("admin");
    apiKeyClient = asApiKey();

    releasableRequisitionDto.setRequisitionId(requisitionId);
    releasableRequisitionDto.setSupplyingDepotId(facilityId);
    releasableDtos.add(releasableRequisitionDto);

    when(requisition.getId()).thenReturn(requisitionId);
    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getFacilityId()).thenReturn(facilityId);
    when(requisition.getSupplyingFacilityId()).thenReturn(facilityId);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);

    when(securityContext.getAuthentication()).thenReturn(userClient);
    when(authenticationHelper.getCurrentUser()).thenReturn(user);

    when(authenticationHelper.getRight(REQUISITION_CREATE)).thenReturn(requisitionCreateRight);
    when(authenticationHelper.getRight(REQUISITION_APPROVE)).thenReturn(requisitionApproveRight);
    when(authenticationHelper.getRight(REQUISITION_AUTHORIZE))
        .thenReturn(requisitionAuthorizeRight);
    when(authenticationHelper.getRight(REQUISITION_DELETE)).thenReturn(requisitionDeleteRight);
    when(authenticationHelper.getRight(REQUISITION_VIEW)).thenReturn(requisitionViewRight);
    when(authenticationHelper.getRight(ORDERS_EDIT)).thenReturn(
        requisitionConvertRight);
    when(authenticationHelper.getRight(REQUISITION_TEMPLATES_MANAGE)).thenReturn(
        manageRequisitionTemplateRight);

    when(requisitionRepository.findOne(requisitionId)).thenReturn(requisition);
    when(requisitionRepository.findAll(ImmutableSet.of(requisitionId)))
        .thenReturn(Lists.newArrayList(requisition));

    ReflectionTestUtils.setField(permissionService, "serviceTokenClientId", SERVICE_CLIENT_ID);
    ReflectionTestUtils.setField(permissionService, "apiKeyPrefix", API_KEY_PREFIX);
  }

  @Test
  public void canInitRequisition() {
    hasRight(requisitionCreateRight, true);

    expectValidationSucceeds(permissionService.canInitRequisition(programId, facilityId));

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_CREATE, requisitionCreateRight);
  }

  @Test
  public void cannotInitRequisition() {
    expectMissingPermission(permissionService.canInitRequisition(programId, facilityId),
        REQUISITION_CREATE);
  }

  @Test
  public void canUpdateRequisition() {
    hasRight(requisitionCreateRight, true);

    when(requisition.getStatus()).thenReturn(RequisitionStatus.INITIATED);

    expectValidationSucceeds(permissionService.canUpdateRequisition(requisition));

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_CREATE, requisitionCreateRight);
  }

  @Test
  public void cannotUpdateRequisition() {
    when(requisition.getStatus()).thenReturn(RequisitionStatus.INITIATED);

    expectMissingPermissionToUpdate(permissionService.canUpdateRequisition(requisition),
        RequisitionStatus.INITIATED, REQUISITION_CREATE);
  }

  @Test
  public void canSubmitRequisition() throws Exception {
    hasRight(requisitionCreateRight, true);

    permissionService.canSubmitRequisition(requisition);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_CREATE, requisitionCreateRight);
  }

  @Test
  public void cannotSubmitRequisition() {
    expectMissingPermission(permissionService.canSubmitRequisition(requisition),
        REQUISITION_CREATE);
  }

  @Test
  public void canApproveRequisition() {
    hasRight(requisitionApproveRight, true);

    permissionService.canApproveRequisition(requisition);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_APPROVE, requisitionApproveRight);
  }

  @Test
  public void cannotApproveRequisition() {
    expectMissingPermission(permissionService.canApproveRequisition(requisition),
        REQUISITION_APPROVE);
  }

  @Test
  public void canAuthorizeRequisition() {
    hasRight(requisitionAuthorizeRight, true);

    permissionService.canAuthorizeRequisition(requisition);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_AUTHORIZE, requisitionAuthorizeRight);
  }

  @Test
  public void cannotAuthorizeRequisition() {
    expectMissingPermission(permissionService.canAuthorizeRequisition(requisition),
        REQUISITION_AUTHORIZE);
  }

  @Test
  public void canDeleteInitiatedRequisitionWhenHasCreateRight() {
    hasRight(requisitionDeleteRight, true);
    hasRight(requisitionCreateRight, true);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.INITIATED);

    permissionService.canDeleteRequisition(requisition);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_DELETE, requisitionDeleteRight);
  }

  @Test
  public void cannotDeleteInitiatedRequisitionWhenHasNoCreateRight() {
    hasRight(requisitionDeleteRight, true);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.INITIATED);

    expectMissingPermission(permissionService.canDeleteRequisition(requisition),
        REQUISITION_CREATE);
  }

  @Test
  public void shouldDeleteSkippedRequisitionWhenHasCreateRight() {
    hasRight(requisitionDeleteRight, true);
    hasRight(requisitionCreateRight, true);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.SKIPPED);

    permissionService.canDeleteRequisition(requisition);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_DELETE, requisitionDeleteRight);
  }

  @Test
  public void shouldNotDeleteSkippedRequisitionWhenHasNoCreateRight() {
    hasRight(requisitionDeleteRight, true);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.SKIPPED);

    expectMissingPermission(permissionService.canDeleteRequisition(requisition),
        REQUISITION_CREATE);
  }

  @Test
  public void canDeleteSubmittedRequisitionWhenHasAuthorizeRight() {
    hasRight(requisitionDeleteRight, true);
    hasRight(requisitionAuthorizeRight, true);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);

    permissionService.canDeleteRequisition(requisition);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_DELETE, requisitionDeleteRight);
  }

  @Test
  public void cannotDeleteSubmittedRequisitionWhenHasNoAuthorizeRight() {
    hasRight(requisitionDeleteRight, true);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);

    expectMissingPermission(permissionService.canDeleteRequisition(requisition),
        REQUISITION_AUTHORIZE);
  }

  @Test
  public void cannotDeleteRequisitionWhenHasNoDeleteRight() {
    expectMissingPermission(permissionService.canDeleteRequisition(requisition),
        REQUISITION_DELETE);
  }

  @Test
  public void canViewRequisition() throws Exception {
    hasRight(requisitionViewRight, true);

    permissionService.canViewRequisition(requisitionId);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifySupervisionRight(order, REQUISITION_VIEW, requisitionViewRight);
  }

  @Test
  public void cannotViewRequisition() throws Exception {
    expectMissingPermission(permissionService.canViewRequisition(requisitionId), REQUISITION_VIEW);
  }

  @Test
  public void canConvertToOrder() throws Exception {
    hasRight(requisitionConvertRight, true);

    permissionService.canConvertToOrder(releasableDtos);

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyFulfillmentRight(order, ORDERS_EDIT, requisitionConvertRight);
  }

  @Test
  public void cannotConvertToOrder() throws Exception {
    expectMissingPermission(permissionService.canConvertToOrder(releasableDtos), ORDERS_EDIT);
  }

  @Test
  public void canManageRequisitionTemplate() throws Exception {
    hasRight(manageRequisitionTemplateRight, true);

    permissionService.canManageRequisitionTemplate();

    InOrder order = inOrder(authenticationHelper, userReferenceDataService);
    verifyGeneralAdminRight(order, REQUISITION_TEMPLATES_MANAGE, manageRequisitionTemplateRight);
  }

  @Test
  public void cannotManageRequisitionTemplate() throws Exception {
    expectMissingPermission(permissionService.canManageRequisitionTemplate(),
        REQUISITION_TEMPLATES_MANAGE);
  }

  @Test
  public void serviceLevelTokensShouldHaveAllThePermissions() {
    when(securityContext.getAuthentication()).thenReturn(trustedClient);

    // Requisition permissions
    assertThat(permissionService.canViewRequisition(requisitionId).isSuccess(), is(true));
    assertThat(
        permissionService.canInitOrAuthorizeRequisition(programId, facilityId).isSuccess(),
        is(true)
    );
    assertThat(permissionService.canInitRequisition(programId, facilityId).isSuccess(), is(true));
    assertThat(permissionService.canApproveRequisition(requisition).isSuccess(), is(true));
    assertThat(permissionService.canAuthorizeRequisition(requisition).isSuccess(), is(true));
    assertThat(permissionService.canDeleteRequisition(requisition).isSuccess(), is(true));
    assertThat(permissionService.canSubmitRequisition(requisition).isSuccess(), is(true));
    assertThat(permissionService.canUpdateRequisition(requisition).isSuccess(), is(true));
    assertThat(permissionService.canConvertToOrder(releasableDtos).isSuccess(), is(true));

    // Report permissions
    assertThat(permissionService.canViewReports().isSuccess(), is(true));
    assertThat(permissionService.canEditReportTemplates().isSuccess(), is(true));
    assertThat(permissionService.canManageRequisitionTemplate().isSuccess(), is(true));
  }

  @Test
  public void serviceLevelTokensShouldNotHaveAnyPermissionIfIdsDoesNotMatch() {
    when(securityContext.getAuthentication()).thenReturn(apiKeyClient);

    assertThat(
        permissionService.canInitOrAuthorizeRequisition(programId, facilityId).isSuccess(),
        is(false)
    );

    assertThat(permissionService.canViewRequisition(requisitionId).isSuccess(), is(false));
    assertThat(permissionService.canInitRequisition(programId, facilityId).isSuccess(), is(false));
    assertThat(permissionService.canApproveRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canAuthorizeRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canDeleteRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canSubmitRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canUpdateRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canConvertToOrder(releasableDtos).isSuccess(), is(false));

    // Report permissions
    assertThat(permissionService.canViewReports().isSuccess(), is(false));
    assertThat(permissionService.canEditReportTemplates().isSuccess(), is(false));
    assertThat(permissionService.canManageRequisitionTemplate().isSuccess(), is(false));
  }

  @Test
  public void apiKeyTokensShouldNotHaveAllThePermissions() {
    when(securityContext.getAuthentication()).thenReturn(apiKeyClient);

    // Requisition permissions
    assertThat(permissionService.canViewRequisition(requisitionId).isSuccess(), is(false));
    assertThat(
        permissionService.canInitOrAuthorizeRequisition(programId, facilityId).isSuccess(),
        is(false)
    );
    assertThat(permissionService.canInitRequisition(programId, facilityId).isSuccess(), is(false));
    assertThat(permissionService.canApproveRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canAuthorizeRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canDeleteRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canSubmitRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canUpdateRequisition(requisition).isSuccess(), is(false));
    assertThat(permissionService.canConvertToOrder(releasableDtos).isSuccess(), is(false));

    // Report permissions
    assertThat(permissionService.canViewReports().isSuccess(), is(false));
    assertThat(permissionService.canEditReportTemplates().isSuccess(), is(false));
    assertThat(permissionService.canManageRequisitionTemplate().isSuccess(), is(false));
  }
  
  @Test
  public void getPermissionStringsShouldGet() {
    // given
    when(userReferenceDataService.getPermissionStrings(user.getId()))
        .thenReturn(Collections.singletonList("permissionString"));
    
    // when
    List<String> permissionStrings = permissionService.getPermissionStrings();
    
    // then
    assertEquals(1, permissionStrings.size());
    assertEquals("permissionString", permissionStrings.get(0));
  }
  
  @Test
  public void getPermissionStringsShouldReturnEmptyListForServiceTokens() {
    // given
    when(securityContext.getAuthentication()).thenReturn(trustedClient);

    // when
    List<String> permissionStrings = permissionService.getPermissionStrings();

    // then
    assertEquals(0, permissionStrings.size());
  }
  
  private void hasRight(RightDto right, boolean assign) {
    ResultDto<Boolean> resultDto = new ResultDto<>(assign);
    when(userReferenceDataService
        .hasRight(user.getId(), right.getId(), programId, facilityId, null)
    ).thenReturn(resultDto);
    when(userReferenceDataService
        .hasRight(user.getId(), right.getId(), null, null, facilityId)
    ).thenReturn(resultDto);
    when(userReferenceDataService
        .hasRight(user.getId(), right.getId(), null, null, null)
    ).thenReturn(resultDto);
  }

  private void expectValidationSucceeds(ValidationResult validationResult) {
    assertTrue(validationResult.isSuccess());
  }

  private void expectMissingPermission(ValidationResult result, String rightName) {
    assertTrue(result.hasErrors());
    assertEquals(FailureType.NO_PERMISSION, result.getError().getType());
    assertEquals(new Message(ERROR_NO_FOLLOWING_PERMISSION, rightName),
        result.getError().getMessage());
  }

  private void expectMissingPermissionToUpdate(ValidationResult result, RequisitionStatus status,
                                               String rightName) {
    assertTrue(result.hasErrors());
    assertEquals(FailureType.NO_PERMISSION, result.getError().getType());
    assertEquals(new Message(ERROR_NO_FOLLOWING_PERMISSION_FOR_REQUISITION_UPDATE,
        status.toString(), rightName), result.getError().getMessage());
  }

  private void verifySupervisionRight(InOrder order, String rightName, RightDto right) {
    order.verify(authenticationHelper).getCurrentUser();
    order.verify(authenticationHelper).getRight(rightName);
    order.verify(userReferenceDataService)
        .hasRight(user.getId(), right.getId(), programId, facilityId, null);
  }

  private void verifyFulfillmentRight(InOrder order, String rightName, RightDto right) {
    order.verify(authenticationHelper).getCurrentUser();
    order.verify(authenticationHelper).getRight(rightName);
    order.verify(userReferenceDataService).hasRight(user.getId(), right.getId(), null, null,
        facilityId);
  }

  private void verifyGeneralAdminRight(InOrder order, String rightName, RightDto right) {
    order.verify(authenticationHelper).getCurrentUser();
    order.verify(authenticationHelper).getRight(rightName);
    order.verify(userReferenceDataService).hasRight(user.getId(), right.getId(), null, null, null);
  }

}
