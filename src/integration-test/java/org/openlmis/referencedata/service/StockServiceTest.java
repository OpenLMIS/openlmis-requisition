package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.openlmis.referencedata.repository.StockInventoryRepository;
import org.openlmis.referencedata.repository.StockRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class StockServiceTest {

  @Autowired
  private StockService stockService;

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private StockInventoryRepository stockInventoryRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private List<Stock> stocks;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    stocks = new ArrayList<>();
    for ( int stockNumber = 0; stockNumber < 5; stockNumber++ ) {
      stocks.add(generateStock());
    }
  }

  @After
  public void cleanup() {
    stockRepository.deleteAll();
    stockInventoryRepository.deleteAll();
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testSearchStocks() {
    List<Stock> receivedStocks = stockService.searchStocks(
            stocks.get(0).getStockInventory(),
            stocks.get(0).getProduct());

    Assert.assertEquals(1,receivedStocks.size());
    for ( Stock programProduct : receivedStocks ) {
      Assert.assertEquals(
              programProduct.getProduct().getId(),
              stocks.get(0).getProduct().getId());
      Assert.assertEquals(
              programProduct.getStockInventory().getId(),
              stocks.get(0).getStockInventory().getId());
    }
  }

  private Stock generateStock() {
    ProductCategory productCategory = generateProductCategory();
    Product product = generateProduct(productCategory);
    StockInventory stockInventory = generateStockInventory();
    Stock stock = new Stock();
    stock.setStockInventory(stockInventory);
    stock.setProduct(product);
    stockRepository.save(stock);
    return stock;
  }

  private ProductCategory generateProductCategory() {
    Integer instanceNumber = generateInstanceNumber();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("code" + instanceNumber);
    productCategory.setName("vaccine" + instanceNumber);
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);
    return productCategory;
  }

  private Product generateProduct(ProductCategory productCategory) {
    Integer instanceNumber = generateInstanceNumber();
    Product product = new Product();
    product.setCode("code" + instanceNumber);
    product.setPrimaryName("product" + instanceNumber);
    product.setDispensingUnit("unit" + instanceNumber);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory);
    productRepository.save(product);
    return product;
  }

  private StockInventory generateStockInventory() {
    StockInventory stockInventory = new StockInventory();
    stockInventory.setName("name" + generateInstanceNumber());
    stockInventoryRepository.save(stockInventory);
    return stockInventory;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
