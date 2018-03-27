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
import static org.mockito.Mockito.when;

import com.google.common.collect.Maps;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.requisition.DatePhysicalStockCountCompleted;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;

@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionBuilderTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Mock
  private RequisitionDto requisitionDto;

  @Mock
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private BasicRequisitionTemplateDto requisitionTemplateDto;

  private FacilityDto facility = DtoGenerator.of(FacilityDto.class);
  private ProgramDto program = DtoGenerator.of(ProgramDto.class);
  private ProcessingPeriodDto processingPeriodDto = DtoGenerator.of(ProcessingPeriodDto.class);

  private UUID requisitionUuid = UUID.randomUUID();
  private UUID supervisoryNodeUuid = UUID.randomUUID();
  private ZonedDateTime modifiedDate = ZonedDateTime.now();

  private List<RequisitionLineItem.Importer> lineItemDtos = new ArrayList<>();

  private static final String DRAFT_STATUS_MESSAGE = "draft status message";
  private Money pricePerPack;

  @Before
  public void setUp() {
    when(requisitionDto.getId()).thenReturn(requisitionUuid);
    when(requisitionDto.getFacility()).thenReturn(facility);
    when(requisitionDto.getProgram()).thenReturn(program);
    when(requisitionDto.getProcessingPeriod()).thenReturn(processingPeriodDto);
    when(requisitionDto.getSupervisoryNode()).thenReturn(supervisoryNodeUuid);
    when(requisitionDto.getTemplate()).thenReturn(requisitionTemplateDto);
    when(requisitionDto.getRequisitionLineItems()).thenReturn(lineItemDtos);
    when(requisitionDto.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(requisitionDto.getModifiedDate()).thenReturn(modifiedDate);
    when(requisitionDto.getDraftStatusMessage()).thenReturn(DRAFT_STATUS_MESSAGE);
    when(requisitionDto.getEmergency()).thenReturn(false);
    when(requisitionDto.getDatePhysicalStockCountCompleted()).thenReturn(LocalDate.now());
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
    Requisition requisition = RequisitionBuilder
        .newRequisition(facility.getId(), program.getId(), false);

    assertFalse(requisition.getEmergency());
    assertEquals(program.getId(), requisition.getProgramId());
    assertEquals(facility.getId(), requisition.getFacilityId());
  }

  @Test
  public void shouldInitializeRequisitionFromDtoImporterForUpdate() {
    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.INITIATED,
        Maps.newHashMap());

    assertNotNull(requisition);
    assertEquals(processingPeriodDto.getDurationInMonths(),
        requisition.getNumberOfMonthsInPeriod());
    assertEquals(lineItemDtos, requisition.getRequisitionLineItems());
    assertEquals(DRAFT_STATUS_MESSAGE, requisition.getDraftStatusMessage());
    assertEquals(
        new DatePhysicalStockCountCompleted(requisitionDto.getDatePhysicalStockCountCompleted()),
            requisition.getDatePhysicalStockCountCompleted());
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
    prepareLineItem(new RequisitionLineItemDto());

    Requisition requisition = RequisitionBuilder.newRequisition(
            requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.INITIATED,
        getOrderables());

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotSetSkippedIfNotOnTemplate() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(false);
    RequisitionLineItemDto lineItemDto = new RequisitionLineItemDto();
    lineItemDto.setSkipped(true);
    prepareLineItem(lineItemDto);

    RequisitionBuilder
        .newRequisition(requisitionDto, requisitionTemplate, program.getId(),
            RequisitionStatus.INITIATED, getOrderables());
  }

  @Test
  public void shouldNotSetSkippedIfRequisitionStatusIsAuthorized() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.AUTHORIZED,
        getOrderables());

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldNotSetSkippedIfRequisitionStatusIsApproved() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.APPROVED,
        getOrderables());

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldSetSkippedIfRequisitionStatusIsInitiated() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.INITIATED,
        getOrderables());

    assertEquals(true, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldSetSkippedIfRequisitionStatusIsSubmitted() {
    prepareForTestSkipped();

    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.SUBMITTED,
        getOrderables());

    assertEquals(true, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldThrowExceptionWhenProgramOrderableIsNotFound() {
    expectedException.expect(ValidationMessageException.class);
    expectedException.expectMessage(MessageKeys.ERROR_PROGRAM_NOT_FOUND);

    prepareLineItem(new RequisitionLineItemDto());
    RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, UUID.randomUUID(), RequisitionStatus.INITIATED,
        getOrderables());
  }

  @Test
  public void shouldCreateRequisitionWithPricePerPackFromProgramOrderable() {
    prepareLineItem(new RequisitionLineItemDto());
    Requisition requisition = RequisitionBuilder.newRequisition(
        requisitionDto, requisitionTemplate, program.getId(), RequisitionStatus.INITIATED,
        getOrderables());

    assertEquals(pricePerPack, requisition.getRequisitionLineItems().get(0).getPricePerPack());
  }

  private void prepareForTestSkipped() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(true);
    RequisitionLineItemDto lineItemDto = new RequisitionLineItemDto();
    lineItemDto.setSkipped(true);
    prepareLineItem(lineItemDto);
  }

  private void prepareLineItem(RequisitionLineItemDto lineItemDto) {
    pricePerPack = Money.of(CurrencyUnit.GBP, 20);
    lineItemDto.setOrderable(new OrderableDtoDataBuilder()
        .withProgramOrderable(program.getId(), pricePerPack)
        .build());
    when(requisitionDto.getRequisitionLineItems())
        .thenReturn(Collections.singletonList(lineItemDto));
  }

  private Map<UUID, OrderableDto> getOrderables() {
    return requisitionDto
        .getRequisitionLineItems()
        .stream()
        .map(RequisitionLineItem.Importer::getOrderable)
        .collect(Collectors.toMap(OrderableDto::getId, Function.identity()));
  }
}
