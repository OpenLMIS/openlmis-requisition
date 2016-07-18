package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.StockInventory;
import org.springframework.beans.factory.annotation.Autowired;

public class StockInventoryRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<StockInventory> {

  @Autowired
  private StockInventoryRepository stockInventoryRepository;

  StockInventoryRepository getRepository() {
    return this.stockInventoryRepository;
  }

  StockInventory generateInstance() {
    StockInventory stockInventory = new StockInventory();
    stockInventory.setName("stockInventoryName");
    return stockInventory;
  }
}
