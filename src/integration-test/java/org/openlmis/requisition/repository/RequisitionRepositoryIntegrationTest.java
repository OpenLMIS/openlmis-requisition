package org.openlmis.requisition.repository;

import static java.util.Collections.singleton;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Money;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ProductDto;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  @Autowired
  private RequisitionRepository repository;

  @Autowired
  private RequisitionTemplateRepository templateRepository;

  private RequisitionTemplate testTemplate;

  private List<Requisition> requisitions;

  RequisitionRepository getRepository() {
    return this.repository;
  }

  Requisition generateInstance() {
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), RequisitionStatus.INITIATED, getNextInstanceNumber() % 2 == 0);
    requisition.setCreatedDate(LocalDateTime.now().plusDays(requisitions.size()));
    requisition.setSupervisoryNodeId(UUID.randomUUID());
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setTemplate(testTemplate);
    return requisition;
  }

  @Before
  public void setUp() {
    testTemplate = templateRepository.save(new RequisitionTemplate());
    requisitions = new ArrayList<>();
    for (int count = 0; count < 5; ++count) {
      requisitions.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchRequisitionsByAllParameters() {
    Requisition requisition = new Requisition(requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(), requisitions.get(0).getProcessingPeriodId(),
        requisitions.get(0).getStatus(), requisitions.get(0).getEmergency());
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setSupervisoryNodeId(requisitions.get(0).getSupervisoryNodeId());
    requisition.setTemplate(testTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(),
        requisitions.get(0).getCreatedDate().minusDays(1),
        requisitions.get(0).getCreatedDate().plusDays(2),
        requisitions.get(0).getProcessingPeriodId(),
        requisitions.get(0).getSupervisoryNodeId(),
        new RequisitionStatus[]{requisitions.get(0).getStatus()},
        requisitions.get(0).getEmergency());

    assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      assertEquals(
          receivedRequisition.getProgramId(),
          requisitions.get(0).getProgramId());
      assertEquals(
          receivedRequisition.getProcessingPeriodId(),
          requisitions.get(0).getProcessingPeriodId());
      assertEquals(
          receivedRequisition.getFacilityId(),
          requisitions.get(0).getFacilityId());
      assertEquals(
          receivedRequisition.getSupervisoryNodeId(),
          requisitions.get(0).getSupervisoryNodeId());
      assertEquals(
          receivedRequisition.getStatus(),
          requisitions.get(0).getStatus());
      assertTrue(
          receivedRequisition.getCreatedDate().isBefore(
              requisitions.get(0).getCreatedDate().plusDays(2)));
      assertTrue(
          receivedRequisition.getCreatedDate().isAfter(
              requisitions.get(0).getCreatedDate().minusDays(1)));
      assertEquals(
          receivedRequisition.getEmergency(),
          requisitions.get(0).getEmergency()
      );
    }
  }

  @Test
  public void testSearchRequisitionsByFacilityAndProgram() {
    Requisition requisition = new Requisition(requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(), requisitions.get(0).getProcessingPeriodId(),
        requisitions.get(0).getStatus(), false);
    requisition.setCreatedDate(requisitions.get(0).getCreatedDate().plusDays(1));
    requisition.setSupervisoryNodeId(requisitions.get(0).getSupervisoryNodeId());
    requisition.setTemplate(testTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    repository.save(requisition);

    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        requisitions.get(0).getFacilityId(),
        requisitions.get(0).getProgramId(),
        null, null, null, null, null, null);

    assertEquals(2, receivedRequisitions.size());
    for (Requisition receivedRequisition : receivedRequisitions) {
      assertEquals(
          receivedRequisition.getProgramId(),
          requisitions.get(0).getProgramId());
      assertEquals(
          receivedRequisition.getFacilityId(),
          requisitions.get(0).getFacilityId());
    }
  }

  @Test
  public void testSearchRequisitionsByAllParametersNull() {
    List<Requisition> receivedRequisitions = repository.searchRequisitions(
        null, null, null, null, null, null, null, null);

    assertEquals(5, receivedRequisitions.size());
  }

  @Test
  public void testSearchEmergencyRequsitions() throws Exception {
    List<Requisition> emergency = repository.searchRequisitions(
        null, null, null, null, null, null, null, true
    );

    assertEquals(2, emergency.size());
    emergency.forEach(requisition -> assertTrue(requisition.getEmergency()));
  }

  @Test
  public void testSearchStandardRequisitions() throws Exception {
    List<Requisition> standard = repository.searchRequisitions(
        null, null, null, null, null, null, null, false
    );

    assertEquals(3, standard.size());
    standard.forEach(requisition -> assertFalse(requisition.getEmergency()));
  }

  @Test
  public void testSearchRequisitionsByPeriodAndEmergencyFlag() throws Exception {
    requisitions.forEach(requisition -> {
      List<Requisition> found = repository.searchByProcessingPeriodAndType(
          requisition.getProcessingPeriodId(), requisition.getEmergency()
      );

      found.forEach(element -> {
        assertEquals(requisition.getProcessingPeriodId(), element.getProcessingPeriodId());
        assertEquals(requisition.getEmergency(), element.getEmergency());
      });
    });
  }

  @Test
  public void testSearchRequisitionsByTemplate() {
    // given
    RequisitionTemplate nonMatchingTemplate = templateRepository.save(new RequisitionTemplate());
    Requisition nonMatchingRequisition = requisitions.get(0);
    nonMatchingRequisition.setTemplate(nonMatchingTemplate);

    List<Requisition> matchingRequisitions =
        requisitions.stream().skip(1).collect(Collectors.toList());
    matchingRequisitions.forEach(r -> r.setTemplate(testTemplate));

    requisitions.forEach(r -> repository.save(r));

    // when
    List<Requisition> result = repository.findByTemplateId(testTemplate.getId());

    // then
    assertEquals(matchingRequisitions.size(), result.size());
  }

  @Test
  public void shouldPersistWithMoney() {
    Money pricePerPack = new Money("14.57");
    UUID productId = UUID.randomUUID();

    ProductDto programProduct = new ProductDto();
    programProduct.setPricePerPack(pricePerPack);
    programProduct.setProductId(productId);

    ApprovedProductDto ftap = new ApprovedProductDto();
    ftap.setProduct(programProduct);

    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), RequisitionStatus.INITIATED, false);
    requisition.initiate(setUpTemplateWithBeginningBalance(), singleton(ftap), null);

    requisition = repository.save(requisition);
    requisition = repository.findOne(requisition.getId());

    assertEquals(pricePerPack, requisition.getRequisitionLineItems().get(0).getPricePerPack());
  }

  private RequisitionTemplate setUpTemplateWithBeginningBalance() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();
    column.setName(RequisitionLineItem.BEGINNING_BALANCE);
    column.setIsDisplayed(true);

    return templateRepository.save(new RequisitionTemplate(
        Collections.singletonMap(RequisitionLineItem.BEGINNING_BALANCE, column)));
  }
}
