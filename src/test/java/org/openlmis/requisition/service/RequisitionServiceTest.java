package org.openlmis.requisition.service;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionGroupProgramScheduleDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.InvalidPeriodException;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RequisitionGroupProgramScheduleReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionServiceTest {

  private Requisition requisition;

  @Mock
  private ProgramDto program;

  @Mock
  private ProcessingPeriodDto period;

  @Mock
  private ProcessingPeriodDto period2;

  @Mock
  private ProcessingScheduleDto schedule;

  @Mock
  private ProcessingScheduleDto schedule2;

  @Mock
  private RequisitionGroupProgramScheduleDto requisitionGroupProgramSchedule;

  @Mock
  private SupervisoryNodeDto supervisoryNode;

  @Mock
  private RequisitionLineItemService requisitionLineItemService;

  @Mock
  private RequisitionLineItemRepository requisitionLineItemRepository;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private RequisitionTemplateService requisitionTemplateService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private RequisitionGroupProgramScheduleReferenceDataService referenceDataService;

  @InjectMocks
  private RequisitionService requisitionService;

  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID suggestedPeriodId = UUID.randomUUID();

  @Before
  public void setUp() {
    generateRequisition();
    mockRepositories();
  }

  @Test
  public void shouldDeleteRequisitionIfItIsInitiated() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionService.delete(requisition.getId());
  }

  @Test(expected = InvalidRequisitionStatusException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionService.delete(requisition.getId());
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenDeletingNotExistingRequisition()
      throws RequisitionException {
    UUID deletedRequisitionId = requisition.getId();
    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(null);
    requisitionService.delete(deletedRequisitionId);
  }

  @Test
  public void shouldSkipRequisitionIfItIsValid() throws RequisitionException {
    when(program.getPeriodsSkippable()).thenReturn(true);
    Requisition skippedRequisition = requisitionService.skip(requisition.getId());

    assertEquals(skippedRequisition.getStatus(), RequisitionStatus.SKIPPED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenSkippingNotSkippableProgram()
      throws RequisitionException {
    when(program.getPeriodsSkippable()).thenReturn(false);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenSkippingNotExistingRequisition()
        throws RequisitionException {
    when(requisitionRepository
          .findOne(requisition.getId()))
          .thenReturn(null);
    requisitionService.skip(requisition.getId());
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsAuthorized() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenRejectingRequisitionWithStatusApproved()
        throws RequisitionException {
    requisition.setStatus(RequisitionStatus.APPROVED);
    requisitionService.reject(requisition.getId());
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenRejectingNotExistingRequisition()
        throws RequisitionException {
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);
    requisitionService.reject(requisition.getId());
  }

  @Test
  public void shouldGetAuthorizedRequisitions() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setProgram(program.getId());

    when(requisitionRepository
        .searchRequisitions(null, program.getId(), null, null, null, null, null))
        .thenReturn(Collections.singletonList(requisition));

    List<Requisition> authorizedRequisitions =
        requisitionService.getAuthorizedRequisitions(program);
    List<Requisition> expected = Collections.singletonList(requisition);

    assertEquals(expected, authorizedRequisitions);
  }

  @Test
  public void shouldGetRequisitionsForApprovalIfUserHasSupervisedPrograms() {

    UUID programId = UUID.randomUUID();
    requisition.setProgram(programId);
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    UserDto user = mock(UserDto.class);
    Set<ProgramDto> supervisedPrograms = new HashSet<>();
    supervisedPrograms.add(program);

    when(user.getSupervisedPrograms()).thenReturn(supervisedPrograms);
    when(program.getId()).thenReturn(programId);
    when(userReferenceDataService.findOne(user.getId()))
        .thenReturn(user);
    when(requisitionRepository
        .searchRequisitions(null, programId, null, null, null, null, null))
        .thenReturn(Collections.singletonList(requisition));

    List<Requisition> requisitionsForApproval =
          requisitionService.getRequisitionsForApproval(user.getId());

    assertEquals(1, requisitionsForApproval.size());
    assertEquals(requisitionsForApproval.get(0), requisition);
  }

  @Test
  public void shouldInitiateRequisitionIfItNotAlreadyExist() throws RequisitionException {
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setColumnsMap(
        ImmutableMap.of("beginningBalance", new RequisitionTemplateColumn())
    );

    requisition.setStatus(null);
    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(null);
    when(referenceDataService.searchByProgramAndFacility(
        requisition.getProgram(),
        requisition.getFacility()))
        .thenReturn(requisitionGroupProgramSchedule);
    when(period.getProcessingSchedule()).thenReturn(schedule);
    when(period.getStartDate()).thenReturn(LocalDate.of(2016, 8, 1));
    when(requisitionGroupProgramSchedule.getProcessingSchedule()).thenReturn(schedule);
    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(mock(FacilityDto.class));
    when(programReferenceDataService.findOne(programId)).thenReturn(mock(ProgramDto.class));
    when(requisitionTemplateService.searchRequisitionTemplates(programId))
        .thenReturn(Arrays.asList(requisitionTemplate));

    when(requisitionLineItemService.initiateRequisitionLineItemFields(
        any(Requisition.class), any(RequisitionTemplate.class)))
        .thenAnswer(invocation -> {
          Requisition req = (Requisition) invocation.getArguments()[0];
          req.setRequisitionLineItems(Lists.newArrayList());

          return null;
        });

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false
    );

    assertEquals(initiatedRequisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = IllegalArgumentException.class)
  public void shouldThrowExceptionWhenInitiatingEmptyRequisition()
      throws RequisitionException {
    requisitionService.initiate(null, null, null, null);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenInitiatingAlreadyExistingRequisition()
      throws RequisitionException {
    doReturn(null)
        .when(facilityReferenceDataService)
        .findOne(any(UUID.class));

    doReturn(null)
        .when(programReferenceDataService)
        .findOne(any(UUID.class));

    requisitionService.initiate(programId, facilityId, suggestedPeriodId, false);
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.APPROVED);
    List<Requisition> requisitions = Collections.singletonList(requisition);
    List<Requisition> expectedRequisitions = requisitionService
          .releaseRequisitionsAsOrder(requisitions);
    assertEquals(RequisitionStatus.RELEASED, expectedRequisitions.get(0).getStatus());
  }

  @Test
  public void shouldFindRequisitionIfItExists() {
    when(requisitionRepository.searchRequisitions(
        requisition.getFacility(),
        requisition.getProgram(),
        requisition.getCreatedDate().minusDays(2),
        requisition.getCreatedDate().plusDays(2),
        requisition.getProcessingPeriod(),
        requisition.getSupervisoryNode(),
        requisition.getStatus()))
        .thenReturn(Collections.singletonList(requisition));

    List<Requisition> receivedRequisitions = requisitionService.searchRequisitions(
          requisition.getFacility(),
          requisition.getProgram(),
          requisition.getCreatedDate().minusDays(2),
          requisition.getCreatedDate().plusDays(2),
          requisition.getProcessingPeriod(),
          requisition.getSupervisoryNode(),
          requisition.getStatus());

    assertEquals(1, receivedRequisitions.size());
    assertEquals(
          receivedRequisitions.get(0).getFacility(),
          requisition.getFacility());
    assertEquals(
          receivedRequisitions.get(0).getProgram(),
          requisition.getProgram());
    assertTrue(
          receivedRequisitions.get(0).getCreatedDate().isAfter(
                requisition.getCreatedDate().minusDays(2)));
    assertTrue(
          receivedRequisitions.get(0).getCreatedDate().isBefore(
                requisition.getCreatedDate().plusDays(2)));
    assertEquals(
          receivedRequisitions.get(0).getProcessingPeriod(),
          requisition.getProcessingPeriod());
    assertEquals(
          receivedRequisitions.get(0).getSupervisoryNode(),
          requisition.getSupervisoryNode());
    assertEquals(
          receivedRequisitions.get(0).getStatus(),
          requisition.getStatus());
  }

  @Test(expected = InvalidPeriodException.class)
  public void shouldThrowExceptionWhenInitiatingReqPeriodDoesNotBelongToTheSameScheduleAsProgram()
        throws RequisitionException {
    requisition.setStatus(null);
    when(requisitionRepository
          .findOne(requisition.getId()))
          .thenReturn(null);
    when(period.getProcessingSchedule()).thenReturn(schedule);
    when(requisitionGroupProgramSchedule.getProcessingSchedule()).thenReturn(schedule2);
    requisitionService.initiateRequisition(requisition);
  }

  @Ignore
  @Test
  public void shouldFilterPeriods() {
    period.setStartDate(LocalDate.of(2016, 8, 6));
    period2.setStartDate(LocalDate.of(2016, 8, 2));
    when(referenceDataService.searchByProgramAndFacility(
          requisition.getProgram(),
          requisition.getFacility()))
          .thenReturn(requisitionGroupProgramSchedule);
    when(requisitionGroupProgramSchedule.getProcessingSchedule())
          .thenReturn(schedule);
    when(periodReferenceDataService.search(
          schedule.getId(),
          LocalDate.of(2016, 8, 1)))
          .thenReturn(Arrays.asList(period, period2));

    List<ProcessingPeriodDto> filteredPeriods = requisitionService.filterPeriods(
          requisition.getProgram(),
          requisition.getFacility(),
          LocalDate.of(2016, 8, 1),
          requisition.getEmergency());

    assertEquals(2, filteredPeriods.size());
    assertEquals(filteredPeriods.get(0), period2);
  }

  @Test
  public void shouldValidateProcessingPeriodsForRequisition() throws RequisitionException {
    Requisition req = generateRequisition();
    req.setStatus(null);
    when(requisitionRepository
          .findOne(req.getId()))
          .thenReturn(null);
    when(requisitionRepository
          .findAll())
          .thenReturn(Arrays.asList(requisition));
    when(referenceDataService.searchByProgramAndFacility(
          requisition.getProgram(),
          requisition.getFacility()))
          .thenReturn(requisitionGroupProgramSchedule);
    when(period.getProcessingSchedule()).thenReturn(schedule);
    when(period.getStartDate()).thenReturn(LocalDate.of(2016, 8, 1));
    when(requisitionGroupProgramSchedule.getProcessingSchedule()).thenReturn(schedule2);

    when(requisitionService.filterPeriods(
          requisition.getProgram(),
          requisition.getFacility(),
          LocalDate.of(2016, 8, 1),
          requisition.getEmergency()))
          .thenReturn(Arrays.asList(period));
    Boolean result = requisitionService.validatePeriodForRequisition(req);

    assertFalse(result);
  }

  private Requisition generateRequisition() {
    requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setEmergency(false);
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(mock(RequisitionLineItem.class));
    requisition.setRequisitionLineItems(requisitionLineItems);
    UUID facilityId = UUID.randomUUID();
    requisition.setFacility(facilityId);
    UUID programId = UUID.randomUUID();
    requisition.setProgram(programId);
    UUID processingPeriodId = UUID.randomUUID();
    requisition.setProcessingPeriod(processingPeriodId);
    return requisition;
  }

  private void mockRepositories() {
    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(requisition);
    when(requisitionRepository
        .save(requisition))
        .thenReturn(requisition);
    when(requisitionRepository
        .save(requisition))
        .thenReturn(requisition);
    when(programReferenceDataService
          .findOne(any()))
          .thenReturn(program);
    when(periodReferenceDataService
          .findOne(any()))
          .thenReturn(period);
    when(referenceDataService
          .search(any()))
          .thenReturn(requisitionGroupProgramSchedule);
  }
}
