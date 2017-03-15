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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.Message;
import org.openlmis.utils.RequisitionExportHelper;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionReportDtoBuilderTest {

  private static final String SYSTEM = "SYSTEM";
  private static final Money TOTAL_COST = Money.of(CurrencyUnit.EUR, 15.6);
  private static final Money FS_TOTAL_COST = Money.of(CurrencyUnit.EUR, 3);
  private static final Money NFS_TOTAL_COST = Money.of(CurrencyUnit.EUR, 22.8);

  @Mock
  private RequisitionExportHelper exportHelper;

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Mock
  private MessageService messageService;

  @Mock
  private Requisition requisition;

  @Mock
  private RequisitionDto requisitionDto;

  @Mock
  private UserDto user1;

  @Mock
  private UserDto user2;

  @Mock
  private List<RequisitionLineItemDto> fullSupplyDtos;

  @Mock
  private List<RequisitionLineItemDto> nonFullSupplyLineDtos;

  @Mock
  private List<RequisitionLineItem> fullSupply;

  @Mock
  private List<RequisitionLineItem> nonFullSupply;

  private UUID userId1 = UUID.randomUUID();
  private UUID userId2 = UUID.randomUUID();

  @InjectMocks
  private RequisitionReportDtoBuilder requisitionReportDtoBuilder =
      new RequisitionReportDtoBuilder();

  @Before
  public void setUp() {
    when(user1.getId()).thenReturn(userId1);
    when(user2.getId()).thenReturn(userId2);
    when(userReferenceDataService.findOne(userId1)).thenReturn(user1);
    when(userReferenceDataService.findOne(userId2)).thenReturn(user2);
    when(requisitionDtoBuilder.build(requisition)).thenReturn(requisitionDto);

    when(requisition.getNonSkippedFullSupplyRequisitionLineItems())
        .thenReturn(fullSupply);
    when(requisition.getNonSkippedNonFullSupplyRequisitionLineItems())
        .thenReturn(nonFullSupply);
    when(exportHelper.exportToDtos(fullSupply))
        .thenReturn(fullSupplyDtos);
    when(exportHelper.exportToDtos(nonFullSupply))
        .thenReturn(nonFullSupplyLineDtos);

    Message msg = new Message(MessageKeys.STATUS_CHANGE_USER_SYSTEM);
    when(messageService.localize(msg))
      .thenReturn(msg. new LocalizedMessage(SYSTEM));

    when(requisition.getTotalCost()).thenReturn(TOTAL_COST);
    when(requisition.getFullSupplyTotalCost()).thenReturn(FS_TOTAL_COST);
    when(requisition.getNonFullSupplyTotalCost()).thenReturn(NFS_TOTAL_COST);
  }

  @Test
  public void shouldBuildDtoWithoutStatusChanges() {
    RequisitionReportDto dto = requisitionReportDtoBuilder.build(requisition);

    commonReportDtoAsserts(dto);
    assertNull(dto.getInitiatedBy());
    assertNull(dto.getInitiatedDate());
    assertNull(dto.getSubmittedBy());
    assertNull(dto.getSubmittedDate());
    assertNull(dto.getAuthorizedBy());
    assertNull(dto.getAuthorizedDate());
  }

  @Test
  public void shouldBuildDtoWithStatusChanges() {
    ZonedDateTime initDt = ZonedDateTime.now().minusDays(11);
    ZonedDateTime submitDt = ZonedDateTime.now().minusDays(6);
    ZonedDateTime authorizeDt = ZonedDateTime.now().minusDays(2);
    StatusChange initStatusChange = mock(StatusChange.class);
    StatusChange submitStatusChange = mock(StatusChange.class);
    StatusChange authorizeStatusChange = mock(StatusChange.class);
    when(initStatusChange.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(initStatusChange.getCreatedDate()).thenReturn(initDt);
    when(initStatusChange.getAuthorId()).thenReturn(userId1);
    when(submitStatusChange.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);
    when(submitStatusChange.getCreatedDate()).thenReturn(submitDt);
    when(submitStatusChange.getAuthorId()).thenReturn(userId2);
    when(authorizeStatusChange.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);
    when(authorizeStatusChange.getCreatedDate()).thenReturn(authorizeDt);
    when(authorizeStatusChange.getAuthorId()).thenReturn(userId1);
    List<StatusChange> statusChanges = new ArrayList<>();
    statusChanges.add(initStatusChange);
    statusChanges.add(submitStatusChange);
    statusChanges.add(authorizeStatusChange);
    when(requisition.getStatusChanges()).thenReturn(statusChanges);

    RequisitionReportDto dto = requisitionReportDtoBuilder.build(requisition);

    commonReportDtoAsserts(dto);
    assertEquals(user1, dto.getInitiatedBy());
    assertEquals(initDt, dto.getInitiatedDate());
    assertEquals(user2, dto.getSubmittedBy());
    assertEquals(submitDt, dto.getSubmittedDate());
    assertEquals(user1, dto.getAuthorizedBy());
    assertEquals(authorizeDt, dto.getAuthorizedDate());
  }

  @Test
  public void shouldBuildDtoWithSystemStatusChange() {
    ZonedDateTime now = ZonedDateTime.now();
    StatusChange initStatusChange = mock(StatusChange.class);
    when(initStatusChange.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(initStatusChange.getCreatedDate()).thenReturn(now);
    List<StatusChange> statusChanges = Collections.singletonList(initStatusChange);
    when(requisition.getStatusChanges()).thenReturn(statusChanges);

    RequisitionReportDto dto = requisitionReportDtoBuilder.build(requisition);

    commonReportDtoAsserts(dto);
    assertNull(dto.getSubmittedBy());
    assertNull(dto.getSubmittedDate());
    assertNull(dto.getAuthorizedBy());
    assertNull(dto.getAuthorizedDate());

    assertEquals(now, dto.getInitiatedDate());
    UserDto fakeUser = dto.getInitiatedBy();
    assertNotNull(fakeUser);
    assertEquals(SYSTEM, fakeUser.getFirstName());
    assertNull(fakeUser.getLastName());
    assertEquals(SYSTEM, fakeUser.getUsername());
  }

  private void commonReportDtoAsserts(RequisitionReportDto dto) {
    assertEquals(requisitionDto, dto.getRequisition());
    assertEquals(fullSupplyDtos, dto.getFullSupply());
    assertEquals(nonFullSupplyLineDtos, dto.getNonFullSupply());
    assertEquals(TOTAL_COST, dto.getTotalCost());
    assertEquals(FS_TOTAL_COST, dto.getFullSupplyTotalCost());
    assertEquals(NFS_TOTAL_COST, dto.getNonFullSupplyTotalCost());
  }
}
