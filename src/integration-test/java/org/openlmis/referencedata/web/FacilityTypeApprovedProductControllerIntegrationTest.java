package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.FacilityTypeApprovedProduct;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.FacilityTypeApprovedProductRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class FacilityTypeApprovedProductControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/facilityTypeApprovedProducts";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

  @Autowired
  private FacilityTypeApprovedProductRepository repository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ProgramProductRepository programProductRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private ProgramRepository programRepository;

  private FacilityTypeApprovedProduct facilityTypeAppProd = new FacilityTypeApprovedProduct();

  @Before
  public void setUp() {
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("productCategoryCode");
    productCategory.setName("productCategoryName");
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);

    Product product = new Product();
    product.setPrimaryName("productName");
    product.setCode("productCode");
    product.setDispensingUnit("pill");
    product.setDosesPerDispensingUnit(1);
    product.setPackSize(12);
    product.setPackRoundingThreshold(10);
    product.setRoundToZero(true);
    product.setActive(true);
    product.setFullSupply(false);
    product.setTracer(false);
    product.setProductCategory(productCategory);
    productRepository.save(product);

    Program program = new Program();
    program.setCode("programCode");
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    ProgramProduct programProduct = new ProgramProduct();
    programProduct.setProductCategory(productCategory);
    programProduct.setProgram(program);
    programProduct.setProduct(product);
    programProduct.setDosesPerMonth(2);
    programProduct.setActive(true);
    programProduct.setFullSupply(true);
    programProductRepository.save(programProduct);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("facilityType");
    facilityTypeRepository.save(facilityType);

    facilityTypeAppProd.setFacilityType(facilityType);
    facilityTypeAppProd.setProgramProduct(programProduct);
    facilityTypeAppProd.setMaxMonthsOfStock(6.00);
    repository.save(facilityTypeAppProd);
  }

  @Test
  public void shouldDeleteFacilityTypeApprovedProduct() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityTypeAppProd.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(repository.exists(facilityTypeAppProd.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateFacilityTypeApprovedProduct() {

    repository.delete(facilityTypeAppProd);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(facilityTypeAppProd)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateFacilityTypeApprovedProduct() {

    facilityTypeAppProd.setMaxMonthsOfStock(9.00);

    FacilityTypeApprovedProduct response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityTypeAppProd.getId())
          .body(facilityTypeAppProd)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityTypeApprovedProduct.class);

    assertTrue(response.getMaxMonthsOfStock().equals(9.00));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewFacilityTypeApprovedProductIfDoesNotExists() {

    repository.delete(facilityTypeAppProd);
    facilityTypeAppProd.setMaxMonthsOfStock(9.00);

    FacilityTypeApprovedProduct response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(facilityTypeAppProd)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityTypeApprovedProduct.class);

    assertTrue(response.getMaxMonthsOfStock().equals(9.00));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllFacilityTypeApprovedProducts() {

    FacilityTypeApprovedProduct[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityTypeApprovedProduct[].class);

    Iterable<FacilityTypeApprovedProduct> facilityTypeApprovedProducts = Arrays.asList(response);
    assertTrue(facilityTypeApprovedProducts.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenFacilityTypeApprovedProduct() {

    FacilityTypeApprovedProduct response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityTypeAppProd.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityTypeApprovedProduct.class);

    assertTrue(repository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentFacilityTypeApprovedProduct() {

    repository.delete(facilityTypeAppProd);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityTypeAppProd.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
