package org.openlmis.referencedata.repository;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Iterator;

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

  /**
   * Prepare the test environment.
   */
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
    productRepository.save( this.product);
    this.productCategory.setCode("code3");
    this.productCategory.setName("vaccine");
    this.productCategory.setDisplayOrder(1);
    productCategoryRepositoryRepository.save( this.productCategory);
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


  @Test
  public void testGetAllProgramProducts() {
    ProgramProduct testProgram = this.generateInstance();
    testProgram.setFullSupply(true);
    ProgramProduct testProgram2 = this.generateInstance();
    testProgram.setFullSupply(false);
    programProductRepository.save(testProgram);
    programProductRepository.save(testProgram2);
    assertEquals(2, getIterableSize(programProductRepository.findByProgram(program)));
  }

  @Test
  public void testGetProductsWhenFullSupplyTrue() {
    ProgramProduct testProgram = this.generateInstance();
    testProgram.setFullSupply(true);
    ProgramProduct testProgram2 = this.generateInstance();
    testProgram2.setFullSupply(false);
    programProductRepository.save(testProgram);
    programProductRepository.save(testProgram2);
    Iterable<ProgramProduct> programProductIterable =
            programProductRepository.findByProgramAndFullSupply(program,true);
    assertEquals(1, getIterableSize(programProductIterable));
    for (ProgramProduct programProduct : programProductIterable ) {
      assertEquals(true, programProduct.isFullSupply());
    }
  }

  @Test
  public void testGetProductsWhenFullSupplyFalse() {
    ProgramProduct testProgram = this.generateInstance();
    testProgram.setFullSupply(true);
    ProgramProduct testProgram2 = this.generateInstance();
    testProgram2.setFullSupply(false);
    ProgramProduct testProgram3 = this.generateInstance();
    testProgram3.setFullSupply(false);
    programProductRepository.save(testProgram);
    programProductRepository.save(testProgram2);
    programProductRepository.save(testProgram3);
    Iterable<ProgramProduct> programProductIterable =
            programProductRepository.findByProgramAndFullSupply(program,false);
    assertEquals(2, getIterableSize(programProductIterable));
    for (ProgramProduct programProduct : programProductIterable ) {
      assertEquals(false, programProduct.isFullSupply());
    }
  }

  private int getIterableSize(Iterable iterable) {
    int iterableSize = 0;
    Iterator iterator = iterable.iterator();
    while (iterator.hasNext()) {
      iterableSize++;
      iterator.next();
    }
    return iterableSize;
  }
}
