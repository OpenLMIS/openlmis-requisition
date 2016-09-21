package org.openlmis.requisition.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineItemRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionLineItem> {

  @Autowired
  private RequisitionLineItemRepository repository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private List<RequisitionLineItem> requisitionLineItems;

  @Before
  public void setUp() {
    requisitionLineItems = new ArrayList<>();
    for (int requisitionLineItemsCount = 0; requisitionLineItemsCount < 5;
         requisitionLineItemsCount++) {
      requisitionLineItems.add(repository.save(generateInstance()));
    }
  }

  RequisitionLineItemRepository getRepository() {
    return this.repository;
  }

  RequisitionLineItem generateInstance() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setProduct(UUID.randomUUID());
    requisitionLineItem.setRequisition(generateRequisition());
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);
    return requisitionLineItem;
  }

  @Test
  public void testSearchRequisitionLineItemsByAllParameters() {
    RequisitionLineItem requisitionLineItem = cloneRequisitionLineItem(requisitionLineItems.get(0));
    List<RequisitionLineItem> receivedRequisitionLineItems = repository.searchRequisitionLineItems(
            requisitionLineItem.getRequisition(),
            requisitionLineItem.getProduct());

    Assert.assertEquals(2, receivedRequisitionLineItems.size());
    for (RequisitionLineItem receivedRequisitionLineItem : receivedRequisitionLineItems) {
      Assert.assertEquals(
              requisitionLineItem.getRequisition().getId(),
              receivedRequisitionLineItem.getRequisition().getId());
      Assert.assertEquals(
              requisitionLineItem.getProduct(),
              receivedRequisitionLineItem.getProduct());
    }
  }

  @Test
  public void testSearchRequisitionLineItemsByAllParametersNull() {
    List<RequisitionLineItem> receivedRequisitionLineItems =
        repository.searchRequisitionLineItems(null, null);

    Assert.assertEquals(requisitionLineItems.size(), receivedRequisitionLineItems.size());
  }

  @Test
  public void testSearchRequisitionLineItemsByRequisition() {
    RequisitionLineItem requisitionLineItem = cloneRequisitionLineItem(requisitionLineItems.get(0));
    List<RequisitionLineItem> receivedRequisitionLineItems = repository.searchRequisitionLineItems(
            requisitionLineItem.getRequisition(),
            null);

    Assert.assertEquals(2, receivedRequisitionLineItems.size());
    for (RequisitionLineItem receivedRequisitionLineItem : receivedRequisitionLineItems) {
      Assert.assertEquals(
              requisitionLineItem.getRequisition().getId(),
              receivedRequisitionLineItem.getRequisition().getId());
    }
  }

  private RequisitionLineItem cloneRequisitionLineItem(RequisitionLineItem requisitionLineItem) {
    RequisitionLineItem clonedRequisitionLineItem = new RequisitionLineItem();
    clonedRequisitionLineItem.setProduct(requisitionLineItem.getProduct());
    clonedRequisitionLineItem.setRequisition(requisitionLineItem.getRequisition());
    clonedRequisitionLineItem.setRequestedQuantity(requisitionLineItem.getRequestedQuantity());
    clonedRequisitionLineItem.setStockOnHand(requisitionLineItem.getStockOnHand());
    clonedRequisitionLineItem.setTotalConsumedQuantity(
        requisitionLineItem.getTotalConsumedQuantity());
    clonedRequisitionLineItem.setBeginningBalance(requisitionLineItem.getBeginningBalance());
    clonedRequisitionLineItem.setTotalReceivedQuantity(
        requisitionLineItem.getTotalReceivedQuantity());
    clonedRequisitionLineItem.setTotalLossesAndAdjustments(
            requisitionLineItem.getTotalLossesAndAdjustments());
    repository.save(clonedRequisitionLineItem);
    return clonedRequisitionLineItem;
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
