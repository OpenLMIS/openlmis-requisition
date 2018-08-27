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

package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import guru.nidi.ramltester.junit.RamlMatchers;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StatusMessage;
import org.openlmis.requisition.dto.StatusMessageDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

public class StatusMessageControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String STATUS_MESSAGE_URL = RESOURCE_URL + "/{id}/statusMessages";

  @MockBean
  private StatusMessageRepository statusMessageRepository;

  @Before
  public void setUp() {
    mockUserAuthenticated();
  }

  // GET /api/requisitions/{id}/statusMessages

  @Test
  public void shouldReturnStatusMessagesForRequisition() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.INITIATED);
    StatusMessage message = generateStatusMessage(requisition);

    List<StatusMessage> messages = Collections.singletonList(message);
    given(statusMessageRepository.findByRequisitionId(requisition.getId())).willReturn(messages);
    doReturn(ValidationResult.success()).when(
        permissionService).canViewRequisition(requisition.getId());

    // when
    StatusMessageDto[] result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(STATUS_MESSAGE_URL)
        .then()
        .statusCode(200)
        .extract().as(StatusMessageDto[].class);

    // then
    assertEquals(1, result.length);
    assertEquals(message.getId(), result[0].getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturn403WhenUserHasNoRightsToViewRequisitionOfStatusMessage() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);
    doReturn(ValidationResult.noPermission(PERMISSION_ERROR_MESSAGE, REQUISITION_VIEW))
        .when(permissionService).canViewRequisition(requisition.getId());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(STATUS_MESSAGE_URL)
        .then()
        .statusCode(403);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // Helper methods

  private StatusMessage generateStatusMessage(Requisition requisition) {
    UUID messageId = UUID.randomUUID();

    StatusChange change = new StatusChange();
    change.setId(UUID.randomUUID());

    StatusMessage message = new StatusMessage();
    message.setId(messageId);
    message.setStatusChange(change);
    message.setRequisition(requisition);
    message.setStatus(requisition.getStatus());
    message.setAuthorId(UUID.randomUUID());
    message.setBody("");

    given(statusMessageRepository.findOne(messageId)).willReturn(message);
    return message;
  }
}
