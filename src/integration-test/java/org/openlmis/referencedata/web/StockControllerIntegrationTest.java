package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;

import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
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
import org.openlmis.referencedata.repository.StockRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class StockControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
          + "definition.";

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
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testSearchStocks() {
    Stock[] response = restAssured.given()
            .queryParam("product", stocks.get(0).getProduct().getId())
            .queryParam("access_token", getToken())
            .when()
            .get(BASE_URL + "/api/stocks/search").as(Stock[].class);

    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(1,response.length);
    for ( Stock stock : response ) {
      Assert.assertEquals(
              stock.getProduct().getId(),
              stocks.get(0).getProduct().getId());
    }
  }

  private Stock generateStock() {
    ProductCategory productCategory = generateProductCategory();
    Product product = generateProduct(productCategory);
    Stock stock = new Stock();
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

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
