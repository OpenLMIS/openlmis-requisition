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
    requisition.setProgram(UUID.randomUUID());
    requisition.setFacility(UUID.randomUUID());
    requisition.setProcessingPeriod(UUID.randomUUID());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setCreatedDate(LocalDateTime.now().plusDays(requisitions.size()));
    requisition.setSupervisoryNode(UUID.randomUUID());
    return requisition;
  }

  @Before
  public void setUp() {
    requisitions = new ArrayList<>();
    for (int requisitionLinesCount = 0; requisitionLinesCount < 5; requisitionLinesCount++) {
      requisitions.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchRequisitionsByAllParameters() {
    Requisition requisition = new Requisition();
    requisition.setFacility(requisitions.get(0).getFacility());
    requisition.setProgram(requisitions.get(0).getProgram());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setProcessingPeriod(requisitions.get(0).getProcessingPeriod());
    requisition.setSupervisoryNode(requisitions.get(0).getSupervisoryNode());
    requisition.setStatus(requisitions.get(0).getStatus());
    repository.save(requisition);
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            requisitions.get(0).getFacility(),
            requisitions.get(0).getProgram(),
            requisitions.get(0).getCreatedDate().minusDays(1),
            requisitions.get(0).getCreatedDate().plusDays(2),
            requisitions.get(0).getProcessingPeriod(),
            requisitions.get(0).getSupervisoryNode(),
            requisitions.get(0).getStatus());

    Assert.assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      Assert.assertEquals(
              receivedRequisition.getProgram(),
              requisitions.get(0).getProgram());
      Assert.assertEquals(
              receivedRequisition.getProcessingPeriod(),
              requisitions.get(0).getProcessingPeriod());
      Assert.assertEquals(
              receivedRequisition.getFacility(),
              requisitions.get(0).getFacility());
      Assert.assertEquals(
              receivedRequisition.getSupervisoryNode(),
              requisitions.get(0).getSupervisoryNode());
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
    requisition.setFacility(requisitions.get(0).getFacility());
    requisition.setProgram(requisitions.get(0).getProgram());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setProcessingPeriod(requisitions.get(0).getProcessingPeriod());
    requisition.setSupervisoryNode(requisitions.get(0).getSupervisoryNode());
    requisition.setStatus(requisitions.get(0).getStatus());
    repository.save(requisition);
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            requisitions.get(0).getFacility(),
            requisitions.get(0).getProgram(),
            null, null, null, null, null);

    Assert.assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      Assert.assertEquals(
              receivedRequisition.getProgram(),
              requisitions.get(0).getProgram());
      Assert.assertEquals(
              receivedRequisition.getFacility(),
              requisitions.get(0).getFacility());
    }
  }

  @Test
  public void testSearchRequisitionsByAllParametersNull() {
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
            null, null, null, null, null, null, null);

    Assert.assertEquals(5, receivedRequisitions.size());
  }
}