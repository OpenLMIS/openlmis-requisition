package org.openlmis.referencedata.repository;

import org.junit.After;
import org.junit.Before;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.springframework.beans.factory.annotation.Autowired;

public class ProgramProductRepositoryIntegrationTest
        extends BaseCrudRepositoryIntegrationTest<ProgramProduct> {

  @Autowired
  ProgramProductRepository programProductRepository;

  @Autowired
  ProductRepository productRepository;

  @Autowired
  ProductCategoryRepository productCategoryRepositoryRepository;

  @Autowired
  ProgramRepository programRepository;

  private Program program = new Program();
  private Product product = new Product();
  private ProductCategory productCategory = new ProductCategory();

  @Autowired
  ProgramProductRepository getRepository() {
    return this.programProductRepository;
  }

  @Before
  public void setUp() {
    this.program.setCode("code");
    programRepository.save( this.program);
    this.product.setCode("code2");
    this.product.setPrimaryName("Product #" + getNextInstanceNumber());
    this.product.setDispensingUnit("unit");
    this.product.setDosesPerDispensingUnit(10);
    this.product.setPackSize(1);
    this.product.setPackRoundingThreshold(0);
    this.product.setRoundToZero(false);
    this.product.setActive(true);
    this.product.setFullSupply(true);
    this.product.setTracer(false);
    this.productCategory.setCode("code3");
    this.productCategory.setName("vaccine");
    this.productCategory.setDisplayOrder(1);
    productCategoryRepositoryRepository.save( this.productCategory);
    this.product.setProductCategory(productCategory);
    productRepository.save( this.product);

  }

  ProgramProduct generateInstance() {
    ProgramProduct programProduct = new ProgramProduct();
    programProduct.setProgram(program);
    programProduct.setProduct(product);
    programProduct.setProductCategory(productCategory);
    programProduct.setFullSupply(true);
    programProduct.setActive(true);
    programProduct.setDosesPerMonth(3);
    return programProduct;
  }

  @After
  public void cleanUp() {
    productRepository.deleteAll();
    programProductRepository.deleteAll();
    productCategoryRepositoryRepository.deleteAll();
    programRepository.deleteAll();
  }
}
