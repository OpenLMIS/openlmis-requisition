package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class ProductCategoryControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/productCategories";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
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
  public void shouldFindProductCategories() {
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

  @Test
  public void shouldDeleteProductCategory() {

    ProductCategory productCategory = productCategories.get(4);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", productCategory.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(productCategoryRepository.exists(productCategory.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateProductCategory() {

    ProductCategory productCategory = productCategories.get(4);
    productCategoryRepository.delete(productCategory);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(productCategory)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateProductCategory() {

    ProductCategory productCategory = productCategories.get(4);
    productCategory.setCode("OpenLMIS");

    ProductCategory response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", productCategory.getId())
          .body(productCategory)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProductCategory.class);

    assertEquals(response.getCode(), "OpenLMIS");
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllProductCategories() {

    ProductCategory[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(ProductCategory[].class);

    Iterable<ProductCategory> productCategories = Arrays.asList(response);
    assertTrue(productCategories.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenProductCategory() {

    ProductCategory productCategory = productCategories.get(4);

    ProductCategory response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", productCategory.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProductCategory.class);

    assertTrue(productCategoryRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
