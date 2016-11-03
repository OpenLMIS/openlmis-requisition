package org.openlmis.requisition.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  @Autowired
  private RequisitionRepository repository;

  private List<Requisition> requisitions;

  RequisitionRepository getRepository() {
    return this.repository;
  }

  Requisition generateInstance() {
    Requisition requisition = new Requisition();
    requisition.setProgramId(UUID.randomUUID());
    requisition.setFacilityId(UUID.randomUUID());
    requisition.setProcessingPeriodId(UUID.randomUUID());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setCreatedDate(LocalDateTime.now().plusDays(requisitions.size()));
    requisition.setSupervisoryNodeId(UUID.randomUUID());
    requisition.setEmergency(getNextInstanceNumber() % 2 == 0);
    return requisition;
  }

  @Before
  public void setUp() {
    requisitions = new ArrayList<>();
    for (int count = 0; count < 5; ++count) {
      requisitions.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchRequisitionsByAllParameters() {
    Requisition requisition = new Requisition();
    requisition.setFacilityId(requisitions.get(0).getFacilityId());
    requisition.setProgramId(requisitions.get(0).getProgramId());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setProcessingPeriodId(requisitions.get(0).getProcessingPeriodId());
    requisition.setSupervisoryNodeId(requisitions.get(0).getSupervisoryNodeId());
    requisition.setStatus(requisitions.get(0).getStatus());
    requisition.setEmergency(requisitions.get(0).getEmergency());
    repository.save(requisition);
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            requisitions.get(0).getFacilityId(),
            requisitions.get(0).getProgramId(),
            requisitions.get(0).getCreatedDate().minusDays(1),
            requisitions.get(0).getCreatedDate().plusDays(2),
            requisitions.get(0).getProcessingPeriodId(),
            requisitions.get(0).getSupervisoryNodeId(),
            requisitions.get(0).getStatus(),
            requisitions.get(0).getEmergency());

    Assert.assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      Assert.assertEquals(
              receivedRequisition.getProgramId(),
              requisitions.get(0).getProgramId());
      Assert.assertEquals(
              receivedRequisition.getProcessingPeriodId(),
              requisitions.get(0).getProcessingPeriodId());
      Assert.assertEquals(
              receivedRequisition.getFacilityId(),
              requisitions.get(0).getFacilityId());
      Assert.assertEquals(
              receivedRequisition.getSupervisoryNodeId(),
              requisitions.get(0).getSupervisoryNodeId());
      Assert.assertEquals(
              receivedRequisition.getStatus(),
              requisitions.get(0).getStatus());
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isBefore(
                      requisitions.get(0).getCreatedDate().plusDays(2)));
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isAfter(
                      requisitions.get(0).getCreatedDate().minusDays(1)));
    }
  }

  @Test
  public void testSearchRequisitionsByFacilityAndProgram() {
    Requisition requisition = new Requisition();
    requisition.setFacilityId(requisitions.get(0).getFacilityId());
    requisition.setProgramId(requisitions.get(0).getProgramId());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setProcessingPeriodId(requisitions.get(0).getProcessingPeriodId());
    requisition.setSupervisoryNodeId(requisitions.get(0).getSupervisoryNodeId());
    requisition.setStatus(requisitions.get(0).getStatus());
    repository.save(requisition);
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            requisitions.get(0).getFacilityId(),
            requisitions.get(0).getProgramId(),
            null, null, null, null, null, null);

    Assert.assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      Assert.assertEquals(
              receivedRequisition.getProgramId(),
              requisitions.get(0).getProgramId());
      Assert.assertEquals(
              receivedRequisition.getFacilityId(),
              requisitions.get(0).getFacilityId());
    }
  }

  @Test
  public void testSearchRequisitionsByAllParametersNull() {
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            null, null, null, null, null, null, null, null);

    Assert.assertEquals(5, receivedRequisitions.size());
  }
}
