package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.service.ConfigurationSettingService;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionServiceTest {

  private Requisition requisition;

  @Mock
  private UserRepository userRepository;

  @Mock
  private RequisitionLineService requisitionLineService;

  @Mock
  private RequisitionLineRepository requisitionLineRepository;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private RequisitionRepository requisitionRepository;

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
    boolean deleted = requisitionService.tryDelete(requisition.getId());

    assertTrue(deleted);
  }

  @Test
  public void shouldNotDeleteRequisitionWhenStatusIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    boolean deleted = requisitionService.tryDelete(requisition.getId());

    assertFalse(deleted);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenDeletingNotExistingRequisition()
          throws RequisitionException {
    UUID deletedRequisitionId = requisition.getId();
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    requisitionService.tryDelete(deletedRequisitionId);
  }

  @Test
  public void shouldSkipRequisitionIfItIsValid() throws RequisitionException {
    when(requisition.getProgram().getPeriodsSkippable()).thenReturn(true);
    Requisition skippedRequisition = requisitionService.skip(requisition.getId());

    assertEquals(skippedRequisition.getStatus(), RequisitionStatus.SKIPPED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenSkippingNotSkippableProgram()
          throws RequisitionException {
    when(requisition.getProgram().getPeriodsSkippable()).thenReturn(false);
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
  public void shouldGetAuthorizedRequisitionsIfSupervisoryNodeProvided() {
    SupervisoryNode supervisoryNode = mock(SupervisoryNode.class);

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setSupervisoryNode(supervisoryNode);

    when(requisitionRepository
        .searchRequisitions(null, null, null, null, null, supervisoryNode, null))
        .thenReturn(Arrays.asList(requisition));

    List<Requisition> authorizedRequisitions =
        requisitionService.getAuthorizedRequisitions(supervisoryNode);
    List<Requisition> expected = Arrays.asList(requisition);

    assertEquals(expected, authorizedRequisitions);
  }

  @Test
  public void shouldGetRequisitionsForApprovalIfUserHasSupervisedNode() {
    SupervisoryNode supervisoryNode = mock(SupervisoryNode.class);
    User user = mock(User.class);

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisition.setSupervisoryNode(supervisoryNode);

    UUID userId = UUID.randomUUID();
    when(user.getId()).thenReturn(userId);
    when(user.getSupervisedNode()).thenReturn(supervisoryNode);
    when(userRepository
            .findOne(userId))
            .thenReturn(user);
    when(requisitionRepository
            .searchRequisitions(null, null, null, null, null, supervisoryNode, null))
            .thenReturn(Arrays.asList(requisition));

    List<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(userId);

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

  @Test(expected = RequisitionException.class)
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
  public void shouldAuthorizeRequisitionIfItIsSubmitted() throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionService.authorize(requisition.getId(), requisition, false);

    assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);
    verify(requisitionRepository).save(requisition);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionIfAuthorizationIsConfiguredToBeSkipped()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    when(configurationSettingService
            .getBoolValue("skipAuthorization"))
            .thenReturn(true);
    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingInitiatedRequisition()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test(expected = RequisitionException.class)
  public void shouldThrowExceptionWhenAuthorizingNotExistingRequisition()
      throws RequisitionException {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(null);
    requisitionService.authorize(requisition.getId(), requisition, false);
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() {
    List<Requisition> requisitions = Arrays.asList(requisition);
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
        .thenReturn(Arrays.asList(requisition));

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
    requisition.setFacility(mock(Facility.class));
    requisition.setProcessingPeriod(mock(ProcessingPeriod.class));
    requisition.setProgram(mock(Program.class));
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(mock(SupervisoryNode.class));
    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(mock(RequisitionLine.class));
    requisition.setRequisitionLines(requisitionLines);
    return requisition;
  }

  private void mockRepositories() {
    when(requisitionRepository
            .findOne(requisition.getId()))
            .thenReturn(requisition);
    when(requisitionRepository
            .save(requisition))
            .thenReturn(requisition);
  }
}
