package org.openlmis.requisition.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionLine> {

  @Autowired
  private RequisitionLineRepository repository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private List<RequisitionLine> requisitionLines;

  @Before
  public void setUp() {
    requisitionLines = new ArrayList<>();
    for (int requisitionLinesCount = 0; requisitionLinesCount < 5; requisitionLinesCount++) {
      requisitionLines.add(repository.save(generateInstance()));
    }
  }

  RequisitionLineRepository getRepository() {
    return this.repository;
  }

  RequisitionLine generateInstance() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(UUID.randomUUID());
    requisitionLine.setRequisition(generateRequisition());
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    return requisitionLine;
  }

  @Test
  public void testSearchRequisitionLinesByAllParameters() {
    RequisitionLine requisitionLine = cloneRequisitionLine(requisitionLines.get(0));
    List<RequisitionLine> receivedRequisitionLines = repository.searchRequisitionLines(
            requisitionLine.getRequisition(),
            requisitionLine.getProduct());

    Assert.assertEquals(2, receivedRequisitionLines.size());
    for (RequisitionLine receivedRequisitionLine : receivedRequisitionLines) {
      Assert.assertEquals(
              requisitionLine.getRequisition().getId(),
              receivedRequisitionLine.getRequisition().getId());
      Assert.assertEquals(
              requisitionLine.getProduct(),
              receivedRequisitionLine.getProduct());
    }
  }

  @Test
  public void testSearchRequisitionLinesByAllParametersNull() {
    List<RequisitionLine> receivedRequisitionLines = repository.searchRequisitionLines(null, null);

    Assert.assertEquals(requisitionLines.size(), receivedRequisitionLines.size());
  }

  @Test
  public void testSearchRequisitionLinesByRequisition() {
    RequisitionLine requisitionLine = cloneRequisitionLine(requisitionLines.get(0));
    List<RequisitionLine> receivedRequisitionLines = repository.searchRequisitionLines(
            requisitionLine.getRequisition(),
            null);

    Assert.assertEquals(2, receivedRequisitionLines.size());
    for (RequisitionLine receivedRequisitionLine : receivedRequisitionLines) {
      Assert.assertEquals(
              requisitionLine.getRequisition().getId(),
              receivedRequisitionLine.getRequisition().getId());
    }
  }

  private RequisitionLine cloneRequisitionLine(RequisitionLine requisitionLine) {
    RequisitionLine clonedRequisitionLine = new RequisitionLine();
    clonedRequisitionLine.setProduct(requisitionLine.getProduct());
    clonedRequisitionLine.setRequisition(requisitionLine.getRequisition());
    clonedRequisitionLine.setRequestedQuantity(requisitionLine.getRequestedQuantity());
    clonedRequisitionLine.setStockOnHand(requisitionLine.getStockOnHand());
    clonedRequisitionLine.setTotalConsumedQuantity(requisitionLine.getTotalConsumedQuantity());
    clonedRequisitionLine.setBeginningBalance(requisitionLine.getBeginningBalance());
    clonedRequisitionLine.setTotalReceivedQuantity(requisitionLine.getTotalReceivedQuantity());
    clonedRequisitionLine.setTotalLossesAndAdjustments(
            requisitionLine.getTotalLossesAndAdjustments());
    repository.save(clonedRequisitionLine);
    return clonedRequisitionLine;
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition();
    requisition.setProgram(UUID.randomUUID());
    requisition.setFacility(UUID.randomUUID());
    requisition.setProcessingPeriod(UUID.randomUUID());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);
    return requisition;
  }
}
