package org.openlmis.referencedata.repository;

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
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class ProgramProductRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<ProgramProduct> {

  @Autowired
  private ProgramProductRepository programProductRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private List<ProgramProduct> programProducts;

  ProgramProductRepository getRepository() {
    return this.programProductRepository;
  }

  ProgramProduct generateInstance() {
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
    return programProduct;
  }

  @Before
  public void setUp() {
    programProducts = new ArrayList<>();
    for (int programProductNumber = 0; programProductNumber < 5; programProductNumber++) {
      programProducts.add(programProductRepository.save(generateInstance()));
    }
  }

  @After
  public void cleanUp() {
    productRepository.deleteAll();
    programProductRepository.deleteAll();
    programRepository.deleteAll();
    productCategoryRepository.deleteAll();
  }

  @Test
  public void searchProgramProducts() {
    List<ProgramProduct> receivedProgramProducts =
            programProductRepository.searchProgramProducts(
            programProducts.get(0).getProgram(),
            programProducts.get(0).isFullSupply());
    Assert.assertEquals(1,receivedProgramProducts.size());
    for (ProgramProduct programProduct : receivedProgramProducts) {
      Assert.assertEquals(
              programProduct.getProgram().getId(),
              programProducts.get(0).getProgram().getId());
      Assert.assertEquals(
              programProduct.isFullSupply(),
              programProducts.get(0).isFullSupply());
    }
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("code" + this.getNextInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
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

  private ProductCategory generateProductCategory() {
    Integer instanceNumber = this.getNextInstanceNumber();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("code" + instanceNumber);
    productCategory.setName("vaccine" + instanceNumber);
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);
    return productCategory;
  }
}
