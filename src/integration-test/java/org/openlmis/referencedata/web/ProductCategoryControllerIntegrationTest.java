package org.openlmis.referencedata.web;

import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class ProductCategoryControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  ProductCategoryRepository productCategoryRepository;

  private static final String RESOURCE_URL = BASE_URL + "/api/productCategories";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String CODE = "code";
  private static final String ACCESS_TOKEN = "access_token";

  private Integer currentInstanceNumber;
  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  List<ProductCategory> productCategories;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    productCategories = new ArrayList<>();
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();
    for ( int productCategoriesCount = 0; productCategoriesCount < 5; productCategoriesCount++ ) {
      productCategories.add(generateProductCategory());
    }
  }

  @After
  public void cleanUp() {
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testSearchProductCategories() {
    ProductCategory[] response = restAssured.given()
            .queryParam(CODE, productCategories.get(0).getCode())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL).as(ProductCategory[].class);

    Assert.assertEquals(1,response.length);
    for ( ProductCategory productCategory : response ) {
      Assert.assertEquals(productCategory.getCode(),productCategories.get(0).getCode());
    }
  }

  private ProductCategory generateProductCategory() {
    ProductCategory productCategory = new ProductCategory();
    Integer instanceNumber = generateInstanceNumber();
    productCategory.setName("productCategoryName" + instanceNumber);
    productCategory.setCode("productCategoryCode" + instanceNumber);
    productCategory.setDisplayOrder(instanceNumber);
    productCategoryRepository.save(productCategory);
    return productCategory;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

}
