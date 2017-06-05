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

package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionBuilderTest {

  @Mock
  private RequisitionDto requisitionDto;

  @Mock
  private FacilityDto facilityDto;

  @Mock
  private ProgramDto programDto;

  @Mock
  private ProcessingPeriodDto processingPeriodDto;

  @Mock
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private BasicRequisitionTemplateDto requisitionTemplateDto;

  private UUID requisitionUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();
  private UUID processingPeriodUuid = UUID.randomUUID();
  private UUID programUuid = UUID.randomUUID();
  private UUID supervisoryNodeUuid = UUID.randomUUID();
  private ZonedDateTime modifiedDate = ZonedDateTime.now();

  private List<RequisitionLineItem.Importer> lineItemDtos = new ArrayList<>();

  private static final String DRAFT_STATUS_MESSAGE = "draft status message";
  private static final Integer MONTHS_IN_PERIOD = 5;


  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(requisitionDto.getId()).thenReturn(requisitionUuid);
    when(requisitionDto.getFacility()).thenReturn(facilityDto);
    when(requisitionDto.getProgram()).thenReturn(programDto);
    when(requisitionDto.getProcessingPeriod()).thenReturn(processingPeriodDto);
    when(requisitionDto.getSupervisoryNode()).thenReturn(supervisoryNodeUuid);
    when(requisitionDto.getTemplate()).thenReturn(requisitionTemplateDto);
    when(requisitionDto.getRequisitionLineItems()).thenReturn(lineItemDtos);
    when(requisitionDto.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(requisitionDto.getModifiedDate()).thenReturn(modifiedDate);
    when(requisitionDto.getDraftStatusMessage()).thenReturn(DRAFT_STATUS_MESSAGE);
    when(requisitionDto.getEmergency()).thenReturn(false);

    when(processingPeriodDto.getId()).thenReturn(processingPeriodUuid);
    when(processingPeriodDto.getDurationInMonths()).thenReturn(MONTHS_IN_PERIOD);
    when(facilityDto.getId()).thenReturn(facilityUuid);
    when(programDto.getId()).thenReturn(programUuid);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenFacilityIdIsMissing()
      throws ValidationMessageException {
    RequisitionBuilder.newRequisition(null, UUID.randomUUID(), true);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenProgramIdIsMissing() throws
      ValidationMessageException {
    RequisitionBuilder.newRequisition(UUID.randomUUID(), null, true);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenEmergencyFlagIsMissing() throws
      ValidationMessageException {
    RequisitionBuilder.newRequisition(UUID.randomUUID(), UUID.randomUUID(), null);
  }

  @Test
  public void shouldInitializeRequisitionWithGivenProgramFacilityAndEmergencyFlag() {
    Requisition requisition = RequisitionBuilder.newRequisition(facilityUuid, programUuid, false);

    assertFalse(requisition.getEmergency());
    assertEquals(programUuid, requisition.getProgramId());
    assertEquals(facilityUuid, requisition.getFacilityId());
  }

  @Test
  public void shouldInitializeRequisitionFromDtoImporterForUpdate() {
    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, programUuid, RequisitionStatus.INITIATED);

    assertNotNull(requisition);
    assertEquals(MONTHS_IN_PERIOD, requisition.getNumberOfMonthsInPeriod());
    assertEquals(lineItemDtos, requisition.getRequisitionLineItems());
    assertEquals(DRAFT_STATUS_MESSAGE, requisition.getDraftStatusMessage());
    assertEquals(null, requisition.getId());
    assertEquals(null, requisition.getFacilityId());
    assertEquals(null, requisition.getProgramId());
    assertEquals(null, requisition.getProcessingPeriodId());
    assertEquals(null, requisition.getSupervisoryNodeId());
    assertEquals(null, requisition.getStatus());
    assertEquals(null, requisition.getEmergency());

    // modified date is required for timestamp checks
    assertEquals(modifiedDate, requisition.getModifiedDate());
  }

  @Test
  public void shouldReturnFalseIfSkippedIsNotSetInDto() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(true);
    prepareForTestSkip(new RequisitionLineItemDto());

    Requisition requisition = RequisitionBuilder.newRequisition(
            requisitionDto, requisitionTemplate, null, RequisitionStatus.INITIATED);

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotSetSkippedIfNotOnTemplate() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(false);
    RequisitionLineItemDto lineItemDto = new RequisitionLineItemDto();
    lineItemDto.setSkipped(true);
    prepareForTestSkip(lineItemDto);

    RequisitionBuilder
        .newRequisition(requisitionDto, requisitionTemplate, null, RequisitionStatus.INITIATED);
  }

  @Test
  public void shouldNotSetSkippedIfRequisitionStatusIsAuthorized() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, null, RequisitionStatus.AUTHORIZED);

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldNotSetSkippedIfRequisitionStatusIsApproved() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, null, RequisitionStatus.APPROVED);

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldSetSkippedIfRequisitionStatusIsInitiated() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, null, RequisitionStatus.INITIATED);

    assertEquals(true, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldSetSkippedIfRequisitionStatusIsSubmitted() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, null, RequisitionStatus.SUBMITTED);

    assertEquals(true, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldInitializeRequisitionFromDtoImporterWhenProgramIsNull() {
    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, null, RequisitionStatus.INITIATED);

    assertNotNull(requisition);
    assertNull(requisition.getProgramId());
  }

  private void prepareForTestSkip(RequisitionLineItemDto lineItemDto) {
    OrderableDto orderable = new OrderableDto();
    orderable.setPrograms(Collections.emptySet());
    lineItemDto.setOrderable(orderable);
    when(requisitionDto.getRequisitionLineItems())
        .thenReturn(Collections.singletonList(lineItemDto));
  }

  private void prepareForTestSkipped() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(true);
    RequisitionLineItemDto lineItemDto = new RequisitionLineItemDto();
    lineItemDto.setSkipped(true);
    prepareForTestSkip(lineItemDto);
  }
}
