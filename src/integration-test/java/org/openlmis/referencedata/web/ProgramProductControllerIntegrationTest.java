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
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class ProgramProductControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = BASE_URL + "/api/programProducts";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String PROGRAM = "program";
  private static final String FULLSUPPLY = "fullSupply";
  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
      + "definition.";

  @Autowired
  private ProgramProductRepository programProductRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private List<ProgramProduct> programProducts;

  private Integer currentInstanceNumber;
  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    programProducts = new ArrayList<>();
    for ( int programProductNumber = 0; programProductNumber < 5; programProductNumber++ ) {
      programProducts.add(generateProgramProduct());
    }
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();
  }

  @After
  public void cleanup() {
    programProductRepository.deleteAll();
    programRepository.deleteAll();
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testSearchProgramProducts() {
    ProgramProduct[] response = restAssured.given()
            .queryParam(PROGRAM, programProducts.get(0).getProgram().getId())
            .queryParam(FULLSUPPLY, programProducts.get(0).isFullSupply())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL).as(ProgramProduct[].class);

    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(1,response.length);
    for ( ProgramProduct programProduct : response ) {
      Assert.assertEquals(
              programProduct.getProgram().getId(),
              programProducts.get(0).getProgram().getId());
      Assert.assertEquals(
              programProduct.isFullSupply(),
              programProducts.get(0).isFullSupply());
    }
  }

  private ProgramProduct generateProgramProduct() {
    Program program = generateProgram();
    ProductCategory productCategory = generateProductCategory();
    Product product = generateProduct(productCategory);
    ProgramProduct programProduct = new ProgramProduct();
    programProduct.setProduct(product);
    programProduct.setProductCategory(productCategory);
    programProduct.setProgram(program);
    programProduct.setFullSupply(true);
    programProduct.setActive(true);
    programProduct.setDosesPerMonth(3);
    programProductRepository.save(programProduct);
    return programProduct;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("code" + generateInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
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
