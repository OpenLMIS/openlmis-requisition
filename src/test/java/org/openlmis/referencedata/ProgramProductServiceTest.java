package org.openlmis.referencedata;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.service.ProgramProductService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@Transactional
public class ProgramProductServiceTest {

  @Mock
  private ProgramProductRepository programProductRepository;

  @Mock
  private ProgramRepository programRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCategoryRepository productCategoryRepository;

  @InjectMocks
  @Autowired
  private ProgramProductService programProductService;

  private ProgramProduct programProduct;
  private Program program;
  private Product product;
  private ProductCategory productCategory;

  private List<ProgramProduct> programProducts;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    programProducts = new ArrayList<>();
    programProductService = new ProgramProductService();
    generateInstances();
    initMocks(this);
    mockRepositories();
  }

  @Test
  public void testSearchProgramProducts() {
    List<ProgramProduct> receivedProgramProducts = programProductService.searchProgramProducts(
            programProducts.get(0).getProgram(),
            programProducts.get(0).isFullSupply());
    Assert.assertEquals(1, receivedProgramProducts.size());
    for (ProgramProduct programProduct : receivedProgramProducts) {
      Assert.assertEquals(
              programProduct.getProgram().getId(),
              programProducts.get(0).getProgram().getId());
      Assert.assertEquals(
              programProduct.isFullSupply(),
              programProducts.get(0).isFullSupply());
    }
  }

  private void generateInstances() {
    for (int programProductNumber = 0; programProductNumber < 5; programProductNumber++) {
      programProducts.add(generateProgramProduct());
    }
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

  private void mockRepositories() {
    for (ProgramProduct programProduct : programProducts) {
      when(programProductRepository.findOne(programProduct.getId())).thenReturn(programProduct);
      when(programProductRepository.save(programProduct)).thenReturn(programProduct);
      List<ProgramProduct> matchedProgramProducts = new ArrayList<>();
      for (ProgramProduct programProductWithMatchedProgramAndFullSupply : programProducts) {
        if (programProductWithMatchedProgramAndFullSupply.getProgram().equals(
                programProduct.getProgram()) && programProductWithMatchedProgramAndFullSupply
                .isFullSupply() == programProduct.isFullSupply()) {
          matchedProgramProducts.add(programProductWithMatchedProgramAndFullSupply);
        }
      }
      when(programProductRepository.searchProgramProducts(
              programProduct.getProgram(), programProduct.isFullSupply()))
              .thenReturn(matchedProgramProducts);
    }
    when(programRepository.findOne(program.getId())).thenReturn(program);
    when(productRepository.findOne(product.getId())).thenReturn(product);
    when(productCategoryRepository.findOne(productCategory.getId())).thenReturn(productCategory);

    when(programRepository.save(program)).thenReturn(program);
    when(productRepository.save(product)).thenReturn(product);
    when(productCategoryRepository.save(productCategory)).thenReturn(productCategory);
  }
}
