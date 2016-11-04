package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyList;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.requisition.validate.DraftRequisitionValidator;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.utils.ErrorResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.Errors;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

public class RequisitionControllerTest {

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private PeriodService periodService;

  @Mock
  private Requisition initiatedRequsition;

  @Mock
  private Requisition submittedRequsition;

  @Mock
  private Requisition authorizedRequsition;

  @Mock
  private Requisition approvedRequsition;

  @Mock
  private RequisitionTemplate template;

  @Mock
  private RequisitionValidator validator;

  @Mock
  private DraftRequisitionValidator draftValidator;

  @Mock
  private RequisitionTemplateRepository templateRepository;

  @Mock
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  private UUID programUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();

  private UUID uuid1 = UUID.fromString("00000000-0000-0000-0000-000000000001");
  private UUID uuid2 = UUID.fromString("00000000-0000-0000-0000-000000000002");
  private UUID uuid3 = UUID.fromString("00000000-0000-0000-0000-000000000003");
  private UUID uuid4 = UUID.fromString("00000000-0000-0000-0000-000000000004");
  private UUID uuid5 = UUID.fromString("00000000-0000-0000-0000-000000000005");

  @InjectMocks
  private RequisitionController requisitionController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    List<ProcessingPeriodDto> processingPeriods = generateProcessingPeriods();
    when(initiatedRequsition.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(submittedRequsition.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);
    when(approvedRequsition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    when(periodService.getPeriods(programUuid, facilityUuid, false))
        .thenReturn(processingPeriods);
    when(periodService.getPeriods(programUuid, facilityUuid, true))
        .thenReturn(Collections.singletonList(processingPeriods.get(0)));

    mockRequsitionRepository();
  }

  @Test
  public void shouldReturnCurrentPeriodForEmergency() throws Exception {
    ResponseEntity<?> response =
        requisitionController.getProcessingPeriodIds(programUuid, facilityUuid, true);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    List<ProcessingPeriodDto> periods = (List<ProcessingPeriodDto>) response.getBody();

    verify(periodService).getPeriods(programUuid, facilityUuid, true);
    verifyZeroInteractions(periodService, requisitionRepository);

    assertNotNull(periods);
    assertEquals(1, periods.size());

    List<UUID> periodUuids = periods
        .stream()
        .map(ProcessingPeriodDto::getId)
        .collect(Collectors.toList());

    assertTrue(periodUuids.contains(uuid1));
  }

  @Test
  public void shouldSubmitValidInitiatedRequisition()
          throws RequisitionException, RequisitionTemplateColumnException {
    when(requisitionRepository.findOne(uuid1)).thenReturn(initiatedRequsition);
    when(initiatedRequsition.getProgramId()).thenReturn(uuid2);
    when(templateRepository.getTemplateForProgram(uuid2)).thenReturn(template);

    requisitionController.submitRequisition(uuid1);

    verify(initiatedRequsition).submit(template);
    // we do not update in this endpoint
    verify(initiatedRequsition, never()).updateFrom(any(Requisition.class),
            any(RequisitionTemplate.class), anyList());
  }

  @Test
  public void shouldNotSubmitInvalidRequisition()
          throws RequisitionException, RequisitionTemplateColumnException {
    doAnswer(invocation -> {
      Errors errors = (Errors) invocation.getArguments()[1];
      errors.reject("requisitionLineItems",
          "approvedQuantity is only available during the approval step of the requisition process");
      return null;
    }).when(validator).validate(eq(initiatedRequsition), any(Errors.class));
    when(initiatedRequsition.getId()).thenReturn(uuid1);

    requisitionController.submitRequisition(uuid1);

    verifyNoSubmitOrUpdate(initiatedRequsition);
  }

  @Test
  public void shouldReturnBadRequestWhenRequisitionIdDiffersFromTheOneInUrl() throws Exception {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);
    when(requisitionDto.getId()).thenReturn(uuid1);
    when(requisitionDto.getFacility()).thenReturn(mock(FacilityDto.class));
    when(requisitionDto.getProgram()).thenReturn(mock(ProgramDto.class));
    when(requisitionDto.getProcessingPeriod()).thenReturn(mock(ProcessingPeriodDto.class));

    ResponseEntity responseEntity = requisitionController.updateRequisition(requisitionDto, uuid2);

    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    assertTrue(responseEntity.getBody() instanceof ErrorResponse);
    ErrorResponse errorResponse = (ErrorResponse) responseEntity.getBody();
    assertEquals("Requisition id mismatch", errorResponse.getMessage());
  }

  @Test
  public void shouldUpdateRequisition() throws Exception {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);

    when(requisitionDto.getId()).thenReturn(uuid1);
    when(requisitionDto.getFacility()).thenReturn(mock(FacilityDto.class));
    when(requisitionDto.getProgram()).thenReturn(mock(ProgramDto.class));
    when(requisitionDto.getProcessingPeriod()).thenReturn(mock(ProcessingPeriodDto.class));
    when(initiatedRequsition.getId()).thenReturn(uuid1);

    ResponseEntity responseEntity = requisitionController.updateRequisition(requisitionDto, uuid1);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    verify(initiatedRequsition).updateFrom(any(Requisition.class), anyObject(), anyList());
    verify(requisitionRepository).save(initiatedRequsition);
    verify(stockAdjustmentReasonReferenceDataService)
        .getStockAdjustmentReasonsByProgram(any(UUID.class));
  }

  @Test
  public void shouldNotUpdateWithInvalidRequisition()
      throws RequisitionException, RequisitionTemplateColumnException {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);
    when(requisitionDto.getFacility()).thenReturn(mock(FacilityDto.class));
    when(requisitionDto.getProgram()).thenReturn(mock(ProgramDto.class));
    when(requisitionDto.getProcessingPeriod()).thenReturn(mock(ProcessingPeriodDto.class));

    doAnswer(invocation -> {
      Errors errors = (Errors) invocation.getArguments()[1];
      errors.reject("requisitionLineItems[0].beginningBalance", "Bad argument");

      return null;
    }).when(draftValidator).validate(any(Requisition.class), any(Errors.class));

    requisitionController.updateRequisition(requisitionDto, uuid1);

    verifyNoSubmitOrUpdate(initiatedRequsition);
  }

  private List<ProcessingPeriodDto> generateProcessingPeriods() {
    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(uuid1);
    ProcessingPeriodDto period2 = new ProcessingPeriodDto();
    period2.setId(uuid2);
    ProcessingPeriodDto period3 = new ProcessingPeriodDto();
    period3.setId(uuid3);
    ProcessingPeriodDto period4 = new ProcessingPeriodDto();
    period4.setId(uuid4);
    ProcessingPeriodDto period5 = new ProcessingPeriodDto();
    period5.setId(uuid5);

    List<ProcessingPeriodDto> periods = new ArrayList<>();
    periods.add(period);
    periods.add(period2);
    periods.add(period3);
    periods.add(period4);
    periods.add(period5);

    return periods;
  }

  private void mockRequsitionRepository() {
    when(requisitionRepository.searchByProcessingPeriod(uuid1, false))
            .thenReturn(new ArrayList<>());
    when(requisitionRepository.searchByProcessingPeriod(uuid2, false))
            .thenReturn(Arrays.asList(initiatedRequsition));
    when(requisitionRepository.searchByProcessingPeriod(uuid3, false))
            .thenReturn(Arrays.asList(submittedRequsition));
    when(requisitionRepository.searchByProcessingPeriod(uuid4, false))
            .thenReturn(Arrays.asList(authorizedRequsition));
    when(requisitionRepository.searchByProcessingPeriod(uuid5, false))
            .thenReturn(Arrays.asList(approvedRequsition));
    when(requisitionRepository.save(initiatedRequsition))
            .thenReturn(initiatedRequsition);
    when(requisitionRepository.findOne(uuid1))
            .thenReturn(initiatedRequsition);
  }

  private void verifyNoSubmitOrUpdate(Requisition requisition)
          throws RequisitionException, RequisitionTemplateColumnException {
    verifyZeroInteractions(requisitionService);
    verify(requisition, never()).updateFrom(any(Requisition.class),
            any(RequisitionTemplate.class), anyList());
    verify(requisition, never()).submit(any(RequisitionTemplate.class));
  }
}
