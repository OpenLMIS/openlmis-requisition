package org.openlmis.referencedata.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.repository.StockRepository;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class StockServiceTest {

  @Mock
  private StockRepository stockRepository;

  @InjectMocks
  private StockService stockService;

  @Test
  public void shouldFindStockIfMatchedProduct() {
    Product product = mock(Product.class);
    Stock stock = mock(Stock.class);

    when(stockRepository
            .searchStocks(product))
            .thenReturn(Arrays.asList(stock));

    List<Stock> receivedStocks = stockService.searchStocks(product);

    assertEquals(1, receivedStocks.size());
    assertEquals(stock, receivedStocks.get(0));
  }
}
