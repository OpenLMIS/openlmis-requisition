/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.service.stockmanagement;

import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.RequisitionTemplate;

@RunWith(MockitoJUnitRunner.class)
public class StockOnHandRetrieverBuilderFactoryTest {

  @Mock
  private StockCardSummariesStockManagementService stockCardSummariesStockManagementService;

  @InjectMocks
  private StockOnHandRetrieverBuilderFactory factory;

  @Mock
  private RequisitionTemplate regularTemplate;

  @Mock
  private RequisitionTemplate stockBasedTemplate;

  @Before
  public void setUp() {
    when(regularTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(false);
    when(stockBasedTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(true);
  }

  @Test
  public void shouldCreateBuilderForRegularTemplate() {
    StockOnHandRetrieverBuilder instance = factory.getInstance(regularTemplate);
    assertThat(instance, instanceOf(EmptyStockOnHandRetrieverBuilder.class));
    assertThat(
        instance.getStockCardSummariesService(),
        is(stockCardSummariesStockManagementService)
    );
  }

  @Test
  public void shouldCreateBuilderForStockBasedTemplate() {
    StockOnHandRetrieverBuilder instance = factory.getInstance(stockBasedTemplate);
    assertThat(instance, instanceOf(StandardStockOnHandRetrieverBuilder.class));
    assertThat(
        instance.getStockCardSummariesService(),
        is(stockCardSummariesStockManagementService)
    );
  }
}
