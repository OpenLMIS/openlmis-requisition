package org.openlmis.requisition.web;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class RequisitionControllerTest {

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private Requisition initiatedRequsition;

  @Mock
  private Requisition submittedRequsition;

  @Mock
  private Requisition authorizedRequsition;

  @Mock
  private Requisition approvedRequsition;

  private UUID programUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();

  private UUID uuid1 = UUID.fromString("00000000-0000-0000-0000-000000000001");
  private UUID uuid2 = UUID.fromString("00000000-0000-0000-0000-000000000002");
  private UUID uuid3 = UUID.fromString("00000000-0000-0000-0000-000000000003");
  private UUID uuid4 = UUID.fromString("00000000-0000-0000-0000-000000000004");
  private UUID uuid5 = UUID.fromString("00000000-0000-0000-0000-000000000005");

  private List<ProcessingPeriodDto> processingPeriods;

  @InjectMocks
  private RequisitionController requisitionController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    processingPeriods = generateProcessingPeriods();
    when(initiatedRequsition.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(submittedRequsition.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);
    when(approvedRequsition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    when(periodReferenceDataService.searchByProgramAndFacility(
            programUuid, facilityUuid)).thenReturn(processingPeriods);

    mockRequsitionRepository();
  }

  @Test
  public void shouldReturnOnlyValidPeriodsForRequisitionInitiate() {
    ResponseEntity<?> response =
            requisitionController.getProcessingPeriodIds(programUuid, facilityUuid, false);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    List<ProcessingPeriodDto> periods = (List<ProcessingPeriodDto>) response.getBody();

    verify(periodReferenceDataService).searchByProgramAndFacility(programUuid, facilityUuid);
    verify(requisitionRepository, times(5)).searchByProcessingPeriod(any(UUID.class));

    assertNotNull(periods);
    assertEquals(3, periods.size());

    List<UUID> periodUuids = periods.stream().map(period -> period.getId())
            .collect(Collectors.toList());

    assertTrue(periodUuids.contains(uuid1));
    assertTrue(periodUuids.contains(uuid2));
    assertTrue(periodUuids.contains(uuid3));
  }

  @Test
  public void shouldSubmitValidRequisition() {
    requisitionController.submitRequisition(uuid1);
  }

  @Test
  public void shouldNotSubmitInvalidRequisition() {

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
    when(requisitionRepository.searchByProcessingPeriod(uuid1))
            .thenReturn(new ArrayList<>());
    when(requisitionRepository.searchByProcessingPeriod(uuid2))
            .thenReturn(Arrays.asList(initiatedRequsition));
    when(requisitionRepository.searchByProcessingPeriod(uuid3))
            .thenReturn(Arrays.asList(submittedRequsition));
    when(requisitionRepository.searchByProcessingPeriod(uuid4))
            .thenReturn(Arrays.asList(authorizedRequsition));
    when(requisitionRepository.searchByProcessingPeriod(uuid5))
            .thenReturn(Arrays.asList(approvedRequsition));
  }
}
