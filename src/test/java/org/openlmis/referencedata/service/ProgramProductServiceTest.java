package org.openlmis.referencedata.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class ProgramProductServiceTest {

  @Mock
  private ProgramProductRepository programProductRepository;

  @InjectMocks
  private ProgramProductService programProductService;

  private ProgramProduct programProduct;
  private Program program;
  private Product product;
  private ProductCategory productCategory;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    programProductService = new ProgramProductService();
    generateInstances();
    initMocks(this);
  }

  @Test
  public void testShouldFindProgramProductIfMatchedProgramAndFullSupply() {
    when(programProductRepository
            .searchProgramProducts(programProduct.getProgram(), programProduct.isFullSupply()))
            .thenReturn(Arrays.asList(programProduct));
    List<ProgramProduct> receivedProgramProducts = programProductService.searchProgramProducts(
            programProduct.getProgram(),
            programProduct.isFullSupply());

    assertEquals(1, receivedProgramProducts.size());
    for (ProgramProduct programProduct : receivedProgramProducts) {
      assertEquals(
              programProduct.getProgram().getId(),
              this.programProduct.getProgram().getId());
      assertEquals(
              programProduct.isFullSupply(),
              this.programProduct.isFullSupply());
    }
  }

  private void generateInstances() {
    programProduct = generateProgramProduct();
    program = generateProgram();
    productCategory = generateProductCategory();
    product = generateProduct(productCategory);
  }

  private ProgramProduct generateProgramProduct() {
    program = generateProgram();
    productCategory = generateProductCategory();
    product = generateProduct(productCategory);
    programProduct = new ProgramProduct();
    programProduct.setId(UUID.randomUUID());
    programProduct.setProduct(product);
    programProduct.setProductCategory(productCategory);
    programProduct.setProgram(program);
    programProduct.setFullSupply(true);
    programProduct.setActive(true);
    programProduct.setDosesPerMonth(3);
    return programProduct;
  }

  private Program generateProgram() {
    program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode("code" + generateInstanceNumber());
    program.setPeriodsSkippable(false);
    return program;
  }

  private ProductCategory generateProductCategory() {
    Integer instanceNumber = generateInstanceNumber();
    productCategory = new ProductCategory();
    productCategory.setId(UUID.randomUUID());
    productCategory.setCode("code" + instanceNumber);
    productCategory.setName("vaccine" + instanceNumber);
    productCategory.setDisplayOrder(1);
    return productCategory;
  }

  private Product generateProduct(ProductCategory productCategory) {
    Integer instanceNumber = generateInstanceNumber();
    product = new Product();
    product.setId(UUID.randomUUID());
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
    return product;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
