package org.openlmis.referencedata.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class ProductCategoryControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/productCategories";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String CODE = "code";
  private static final String ACCESS_TOKEN = "access_token";

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private Integer currentInstanceNumber;

  private List<ProductCategory> productCategories;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    productCategories = new ArrayList<>();
    for ( int productCategoriesCount = 0; productCategoriesCount < 5; productCategoriesCount++ ) {
      productCategories.add(generateProductCategory());
    }
  }

  @Test
  public void testShouldFindProductCategories() {
    ProductCategory[] response = restAssured.given()
            .queryParam(CODE, productCategories.get(0).getCode())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(ProductCategory[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( ProductCategory productCategory : response ) {
      assertEquals(productCategory.getCode(), productCategories.get(0).getCode());
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
