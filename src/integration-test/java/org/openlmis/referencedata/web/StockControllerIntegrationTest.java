package org.openlmis.referencedata.web;

import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.openlmis.referencedata.repository.StockInventoryRepository;
import org.openlmis.referencedata.repository.StockRepository;
import org.openlmis.referencedata.service.StockService;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class StockControllerIntegrationTest extends BaseWebIntegrationTest {

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

  private static final String RESOURCE_URL = BASE_URL + "/api/stocks";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String STOCK_INVENTORY = "stockInventory";
  private static final String PRODUCT = "product";

  private List<Stock> stocks;

  private Integer currentInstanceNumber;
  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    stocks = new ArrayList<>();
    for ( int stockNumber = 0; stockNumber < 5; stockNumber++ ) {
      stocks.add(generateStock());
    }
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();
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
    /*Stock[] response = restAssured.given()
            .queryParam(STOCK_INVENTORY, stocks.get(0).getStockInventory().getId())
            .queryParam(PRODUCT, stocks.get(0).getProduct().getId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL).as(Stock[].class);

    Assert.assertEquals(1,response.length);
    for ( Stock stock : response ) {
      Assert.assertEquals(
              stock.getStockInventory().getId(),
              stocks.get(0).getStockInventory().getId());
      Assert.assertEquals(
              stock.getProduct().getId(),
              stocks.get(0).getProduct().getId());*/
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
