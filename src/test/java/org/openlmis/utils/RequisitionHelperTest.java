package org.openlmis.utils;

import static org.junit.Assert.assertEquals;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.domain.LineItemFieldsCalculator;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

@PrepareForTest({LineItemFieldsCalculator.class})
@RunWith(PowerMockRunner.class)
public class RequisitionHelperTest {
  private static final UUID ORDERABLE_PRODUCT_ID = UUID.randomUUID();
  private static final Integer AVERAGE_CONSUMPTION = 5;

  @Test
  public void shouldCalculateAverageConsumption() throws Exception {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    ArrayList<Integer> adjustedConsumptions = new ArrayList<>();
    adjustedConsumptions.addAll(Arrays.asList(1, 2, 3));
    requisitionLineItem.setPreviousAdjustedConsumptions(adjustedConsumptions);
    requisitionLineItem.setAdjustedConsumption(4);

    mockStatic(LineItemFieldsCalculator.class);
    when(LineItemFieldsCalculator.calculateAverageConsumption(Arrays.asList(1, 2, 3, 4)))
        .thenReturn(AVERAGE_CONSUMPTION);

    RequisitionHelper.calculateAverageConsumption(Collections.singletonList(requisitionLineItem));

    assertEquals(AVERAGE_CONSUMPTION, requisitionLineItem.getAverageConsumption());
  }

  @Test
  public void shouldSetPreviousAdjustedConsumptionsWhenOnePreviousRequisition() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);

    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(5);
    previousRequisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition.setRequisitionLineItems(
        Collections.singletonList(previousRequisitionLineItem));

    RequisitionHelper.setPreviousAdjustedConsumptions(
        Collections.singletonList(requisitionLineItem),
        Collections.singletonList(previousRequisition)
    );

    assertEquals(Collections.singletonList(5),
        requisitionLineItem.getPreviousAdjustedConsumptions());
  }

  @Test
  public void shouldSetPreviousAdjustedConsumptionsFromManyPreviousRequisitions() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);

    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(5);
    previousRequisitionLineItem.setOrderableProductId(ORDERABLE_PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition
        .setRequisitionLineItems(Arrays.asList(previousRequisitionLineItem,
            previousRequisitionLineItem, previousRequisitionLineItem));

    RequisitionHelper.setPreviousAdjustedConsumptions(
        Collections.singletonList(requisitionLineItem),
        Collections.singletonList(previousRequisition)
    );

    assertEquals(Arrays.asList(5, 5, 5), requisitionLineItem.getPreviousAdjustedConsumptions());
  }
}