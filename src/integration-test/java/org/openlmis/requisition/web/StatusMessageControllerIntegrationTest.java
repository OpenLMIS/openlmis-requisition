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
import static org.mockito.Mockito.doThrow;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_VIEW;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.StatusMessageDto;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.springframework.http.HttpHeaders;

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

    // when
    StatusMessageDto[] result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
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
    doThrow(mockPermissionException(REQUISITION_VIEW))
        .when(permissionService).canViewRequisition(requisition.getId());

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
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

    StatusMessage message = new StatusMessage();
    message.setId(messageId);
    message.setRequisition(requisition);
    message.setStatus(requisition.getStatus());
    message.setAuthorId(UUID.randomUUID());
    message.setBody("");

    given(statusMessageRepository.findOne(messageId)).willReturn(message);
    return message;
  }
}
