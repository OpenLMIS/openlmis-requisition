package org.openlmis.referencedata.service;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.repository.StockRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class StockService {

  @Autowired
  private StockRepository stockRepository;

  /**
   * Finds Stocks matching all of provided parameters.
   * @param product product of searched Stocks.
   * @return list of all Stocks matching all of provided parameters.
   */
  public List<Stock> searchStocks(
          Product product) {
    return stockRepository.searchStocks(product);
  }
}
