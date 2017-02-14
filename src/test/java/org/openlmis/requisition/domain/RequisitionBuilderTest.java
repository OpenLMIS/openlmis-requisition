package org.openlmis.requisition.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.exception.ValidationMessageException;

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

  private UUID requisitionUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();
  private UUID processingPeriodUuid = UUID.randomUUID();
  private UUID programUuid = UUID.randomUUID();
  private UUID supervisoryNodeUuid = UUID.randomUUID();
  private ZonedDateTime modifiedDate = ZonedDateTime.now();

  private List<RequisitionLineItem.Importer> lineItemDtos = new ArrayList<>();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(requisitionDto.getId()).thenReturn(requisitionUuid);
    when(requisitionDto.getFacility()).thenReturn(facilityDto);
    when(requisitionDto.getProgram()).thenReturn(programDto);
    when(requisitionDto.getProcessingPeriod()).thenReturn(processingPeriodDto);
    when(requisitionDto.getSupervisoryNode()).thenReturn(supervisoryNodeUuid);
    when(requisitionDto.getTemplate()).thenReturn(requisitionTemplate);
    when(requisitionDto.getRequisitionLineItems()).thenReturn(lineItemDtos);
    when(requisitionDto.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(requisitionDto.getModifiedDate()).thenReturn(modifiedDate);

    when(processingPeriodDto.getId()).thenReturn(processingPeriodUuid);
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
  public void shouldInitializeRequisitionFromDtoImporter() {
    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertNotNull(requisition);
    assertEquals(requisitionUuid, requisition.getId());
    assertEquals(facilityUuid, requisition.getFacilityId());
    assertEquals(programUuid, requisition.getProgramId());
    assertEquals(processingPeriodUuid, requisition.getProcessingPeriodId());
    assertEquals(supervisoryNodeUuid, requisition.getSupervisoryNodeId());
    assertEquals(lineItemDtos, requisition.getRequisitionLineItems());
    assertEquals(RequisitionStatus.INITIATED, requisition.getStatus());
    assertEquals(modifiedDate, requisition.getModifiedDate());
  }

  @Test
  public void shouldReturnFalseIfSkippedIsNotSetInDto() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(true);
    prepareForTestSkip(new RequisitionLineItemDto());

    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotSetSkippedIfNotOnTemplate() {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(false);
    RequisitionLineItemDto lineItemDto = new RequisitionLineItemDto();
    lineItemDto.setSkipped(true);
    prepareForTestSkip(lineItemDto);

    RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);
  }

  @Test
  public void shouldNotSetSkippedIfRequisitionStatusIsAuthorized() {
    prepareForTestSkippedDependOnStatus(RequisitionStatus.AUTHORIZED);

    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldNotSetSkippedIfRequisitionStatusIsApproved() {
    prepareForTestSkippedDependOnStatus(RequisitionStatus.APPROVED);

    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertEquals(false, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldSetSkippedIfRequisitionStatusIsInitiated() {
    prepareForTestSkippedDependOnStatus(RequisitionStatus.INITIATED);

    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertEquals(true, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldSetSkippedIfRequisitionStatusIsSubmitted() {
    prepareForTestSkippedDependOnStatus(RequisitionStatus.SUBMITTED);

    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertEquals(true, requisition.getRequisitionLineItems().get(0).getSkipped());
  }

  @Test
  public void shouldInitializeRequisitionFromDtoImporterWhenProgramAndFacilityAreNull() {
    when(requisitionDto.getFacility()).thenReturn(null);
    when(requisitionDto.getProgram()).thenReturn(null);

    Requisition requisition =
        RequisitionBuilder.newRequisition(requisitionDto, requisitionTemplate);

    assertNotNull(requisition);
    assertNull(requisition.getFacilityId());
    assertNull(requisition.getProgramId());
  }

  private void prepareForTestSkip(RequisitionLineItemDto lineItemDto) {
    OrderableDto orderable = new OrderableDto();
    orderable.setPrograms(Collections.emptySet());
    lineItemDto.setOrderable(orderable);
    when(requisitionDto.getRequisitionLineItems())
        .thenReturn(Collections.singletonList(lineItemDto));
  }

  private void prepareForTestSkippedDependOnStatus(RequisitionStatus initiated) {
    when(requisitionTemplate.isColumnDisplayed(RequisitionLineItem.SKIPPED_COLUMN))
        .thenReturn(true);
    when(requisitionDto.getStatus()).thenReturn(initiated);
    RequisitionLineItemDto lineItemDto = new RequisitionLineItemDto();
    lineItemDto.setSkipped(true);
    prepareForTestSkip(lineItemDto);
  }
}
