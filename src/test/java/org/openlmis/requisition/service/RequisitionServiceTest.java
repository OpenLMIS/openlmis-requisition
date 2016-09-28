package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionServiceTest {

  private Requisition requisition;

  @Mock
  private ProgramDto program;

  @Mock
  private  SupervisoryNodeDto supervisoryNode;

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

  @InjectMocks
  private RequisitionService requisitionService;

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
    requisition.setStatus(null);
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    Requisition initiatedRequisition = requisitionService.initiateRequisition(requisition);

    assertEquals(initiatedRequisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = IllegalArgumentException.class)
  public void shouldThrowExceptionWhenInitiatingEmptyRequisition()
          throws RequisitionException {
    requisitionService.initiateRequisition(null);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenInitiatingAlreadyExistingRequisition()
          throws RequisitionException {
    requisitionService.initiateRequisition(requisition);
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

  private Requisition generateRequisition() {
    requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(mock(RequisitionLineItem.class));
    requisition.setRequisitionLineItems(requisitionLineItems);
    return requisition;
  }

  private void mockRepositories() {
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(requisition);
    when(requisitionRepository
            .save(requisition))
            .thenReturn(requisition);
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(program);
  }
}
