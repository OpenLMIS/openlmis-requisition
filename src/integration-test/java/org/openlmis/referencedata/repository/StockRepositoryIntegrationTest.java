package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Stock;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class StockRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Stock> {

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private List<Stock> stocks;

  StockRepository getRepository() {
    return this.stockRepository;
  }

  Stock generateInstance() {
    ProductCategory productCategory = generateProductCategory();
    Product product = generateProduct(productCategory);
    Stock stock = new Stock();
    stock.setProduct(product);
    return stock;
  }

  @Before
  public void setUp() {
    stocks = new ArrayList<>();
    for (int stockNumber = 0; stockNumber < 5; stockNumber++) {
      stocks.add(stockRepository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchStocksByAllParameters() {
    Stock stock = cloneStock(stocks.get(0));
    List<Stock> receivedStocks = stockRepository.searchStocks(
            stock.getProduct());

    Assert.assertEquals(2, receivedStocks.size());
    for (Stock receivedStock : receivedStocks) {
      Assert.assertEquals(
              stock.getProduct().getId(),
              receivedStock.getProduct().getId());
    }
  }

  @Test
  public void testSearchStocksByAllParametersNull() {
    List<Stock> receivedStocks = stockRepository.searchStocks(null);

    Assert.assertEquals(stocks.size(), receivedStocks.size());
  }

  private Stock cloneStock(Stock stock) {
    Stock clonedStock = new Stock();
    clonedStock.setProduct(stock.getProduct());
    stockRepository.save(clonedStock);
    return stock;
  }

  private ProductCategory generateProductCategory() {
    Integer instanceNumber = this.getNextInstanceNumber();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("code" + instanceNumber);
    productCategory.setName("vaccine" + instanceNumber);
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);
    return productCategory;
  }

  private Product generateProduct(ProductCategory productCategory) {
    Integer instanceNumber = this.getNextInstanceNumber();
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
}
