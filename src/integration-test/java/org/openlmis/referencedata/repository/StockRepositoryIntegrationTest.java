package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.Product;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.springframework.beans.factory.annotation.Autowired;

public class StockRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Stock> {

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private StockInventoryRepository stockInventoryRepository;

  @Autowired
  private ProductRepository productRepository;

  private StockInventory stockInventory = new StockInventory();
  private Product product = new Product();

  StockRepository getRepository() {
    return this.stockRepository;
  }

  @Before
  public void setUp() {
    stockInventoryRepository.deleteAll();
    stockInventory.setName("stockInventoryName");
    stockInventoryRepository.save(stockInventory);

    productRepository.deleteAll();
    product.setPrimaryName("productName");
    product.setCode("productCode");
    product.setDispensingUnit("unit");
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    productRepository.save(product);
  }

  Stock generateInstance() {
    Stock stock = new Stock();
    stock.setStockInventory(stockInventory);
    stock.setProduct(product);
    stock.setStoredQuantity(1234L);
    return stock;
  }

  @Test
  public void testFindByStockInventoryAndProduct() {
    stockRepository.save(this.generateInstance());
    Stock stock = stockRepository.findByStockInventoryAndProduct(stockInventory, product);
    long storedQuantity = stock.getStoredQuantity();
    Assert.assertEquals(storedQuantity, 1234L);
  }
}
