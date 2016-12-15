package org.openlmis.requisition.service;

import static java.util.Collections.singletonList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Money;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;

import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;


@SuppressWarnings({"PMD.TooManyMethods"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionLineCalculationServiceTest {

  private static final Money PRICE_PER_PACK = new Money("9");

  private Requisition requisition;
  private OrderableProductDto orderableProductDto;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private ProcessingPeriodDto periodDto1;

  @Mock
  private ProcessingPeriodDto periodDto2;

  @Mock
  private ProcessingPeriodDto periodDto3;

  @Mock
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  @InjectMocks
  private RequisitionLineCalculationService requisitionLineCalculationService;

  private UUID program = UUID.randomUUID();
  private UUID period1 = UUID.randomUUID();
  private UUID period2 = UUID.randomUUID();
  private UUID period3 = UUID.randomUUID();
  private UUID productId = UUID.randomUUID();

  @Before
  public void setUp() {
    generateInstances();
    mockRepositories();
  }

  @Test
  public void shouldExportRequisitionLinesToDtos() {
    RequisitionLineItem requisitionLineItem =
        generateRequisitionLineItemToExport(orderableProductDto.getId());
    List<RequisitionLineItemDto> items =
        requisitionLineCalculationService.exportToDtos(singletonList(requisitionLineItem));
    RequisitionLineItemDto item = items.get(0);
    assertNotNull(item);
    assertEquals(item.getId(), requisitionLineItem.getId());
    assertEquals(item.getOrderableProduct().getId(), requisitionLineItem.getOrderableProductId());
    assertEquals(item.getBeginningBalance(), requisitionLineItem.getBeginningBalance());
    assertEquals(item.getTotalReceivedQuantity(), requisitionLineItem.getTotalReceivedQuantity());
    assertEquals(item.getTotalLossesAndAdjustments(),
        requisitionLineItem.getTotalLossesAndAdjustments());
    assertEquals(item.getStockOnHand(), requisitionLineItem.getStockOnHand());
    assertEquals(item.getRequestedQuantity(), requisitionLineItem.getRequestedQuantity());
    assertEquals(item.getTotalConsumedQuantity(), requisitionLineItem.getTotalConsumedQuantity());
    assertEquals(item.getRequestedQuantityExplanation(),
        requisitionLineItem.getRequestedQuantityExplanation());
    assertEquals(item.getRemarks(), requisitionLineItem.getRemarks());
    assertEquals(item.getApprovedQuantity(), requisitionLineItem.getApprovedQuantity());
    assertEquals(item.getTotalStockoutDays(), requisitionLineItem.getTotalStockoutDays());
    assertEquals(item.getTotal(), requisitionLineItem.getTotal());
    assertEquals(PRICE_PER_PACK, item.getPricePerPack());
  }

  private RequisitionLineItem generateRequisitionLineItemToExport(UUID orderableProductDtoUuid) {
    ProductDto productDto = new ProductDto();
    productDto.setProductId(orderableProductDto.getId());
    productDto.setProgramId(program);
    Set<ProductDto> programs = new HashSet<>();
    programs.add(productDto);
    orderableProductDto.setPrograms(programs);

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(orderableProductDtoUuid);
    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setBeginningBalance(3);
    requisitionLineItem.setTotalReceivedQuantity(4);
    requisitionLineItem.setTotalLossesAndAdjustments(0);
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setRequestedQuantity(5);
    requisitionLineItem.setTotalConsumedQuantity(2);
    requisitionLineItem.setTotal(7);
    requisitionLineItem.setApprovedQuantity(5);
    requisitionLineItem.setTotalStockoutDays(6);
    requisitionLineItem.setPricePerPack(PRICE_PER_PACK);

    return requisitionLineItem;
  }

  private void generateInstances() {
    requisition = createTestRequisition(UUID.randomUUID(), period1, program,
        RequisitionStatus.INITIATED);
    RequisitionLineItem requisitionLineItem = createTestRequisitionLineItem(
        10, 20, requisition
    );

    requisition.setRequisitionLineItems(new ArrayList<>(
            singletonList(requisitionLineItem)));
    orderableProductDto = new OrderableProductDto();
    orderableProductDto.setId(UUID.randomUUID());
  }

  private Requisition createTestRequisition(UUID facility, UUID period,
                                            UUID program,
                                            RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition(facility, program, period, requisitionStatus, false);
    requisition.setId(UUID.randomUUID());
    return requisition;
  }

  private RequisitionLineItem createTestRequisitionLineItem(Integer quantityRequested,
                                                            Integer stockOnHand,
                                                            Requisition requisition) {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setId(UUID.randomUUID());
    requisitionLineItem.setRequestedQuantity(quantityRequested);
    requisitionLineItem.setStockOnHand(stockOnHand);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(productId);
    return requisitionLineItem;
  }

  private void mockRepositories() {
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(new ProgramDto());
    when(periodDto1.getProcessingSchedule())
        .thenReturn(new ProcessingScheduleDto());
    when(periodDto1.getStartDate())
        .thenReturn(LocalDate.of(2016, Month.MARCH, 10));
    when(periodDto2.getStartDate())
        .thenReturn(LocalDate.of(2016, Month.FEBRUARY, 10));
    when(periodDto3.getStartDate())
        .thenReturn(LocalDate.of(2016, Month.JANUARY, 10));
    when(periodDto1.getId())
        .thenReturn(period1);
    when(periodDto2.getId())
        .thenReturn(period2);
    when(periodDto3.getId())
        .thenReturn(period3);
    when(orderableProductReferenceDataService.findOne(orderableProductDto.getId()))
        .thenReturn(orderableProductDto);
  }
}
