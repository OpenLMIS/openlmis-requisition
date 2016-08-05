package org.openlmis.referencedata.service;

import org.junit.After;
//import org.junit.Assert;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class ProgramProductServiceTest {

  @Autowired
  private ProgramProductService programProductService;

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

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    programProducts = new ArrayList<>();
    for ( int programProductNumber = 0; programProductNumber < 5; programProductNumber++ ) {
      programProducts.add(generateProgramProduct());
    }
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
    List<ProgramProduct> receivedProgramProducts = programProductService.searchProgramProducts(
            programProducts.get(0).getProgram(),
            programProducts.get(0).isFullSupply());
    Assert.assertEquals(1,receivedProgramProducts.size());
    for ( ProgramProduct programProduct : receivedProgramProducts ) {
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
