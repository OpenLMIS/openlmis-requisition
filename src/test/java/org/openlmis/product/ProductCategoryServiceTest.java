package org.openlmis.product;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.service.ProductCategoryService;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@Transactional
public class ProductCategoryServiceTest {

  ProductCategoryRepository productCategoryRepository;
  ProductCategoryService productCategoryService;

  private Integer currentInstanceNumber;
  private List<ProductCategory> productCategories;

  @Before
  public void setUp() {
    productCategories = new ArrayList<>();
    currentInstanceNumber = 0;
    productCategoryService = new ProductCategoryService();
    generateInstances();
    mockRepositories();
    initMocks(this);
  }

  @Test
  public void testSearchProductCategories() {
    List<ProductCategory> receivedProductCategories =
            productCategoryService.searchProductCategories(productCategories.get(0).getCode());

    Assert.assertEquals(1, receivedProductCategories.size());
    Assert.assertEquals(
            receivedProductCategories.get(0).getCode(),
            productCategories.get(0).getCode());
  }

  private void generateInstances() {
    for (int instancesCount = 0; instancesCount < 5; instancesCount++) {
      productCategories.add(generateProductCategory());
    }
  }

  private ProductCategory generateProductCategory() {
    ProductCategory productCategory = new ProductCategory();
    Integer instanceNumber = generateInstanceNumber();
    productCategory.setName("productCategoryName" + instanceNumber);
    productCategory.setCode("productCategoryCode" + instanceNumber);
    productCategory.setDisplayOrder(instanceNumber);
    return productCategory;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

  private void mockRepositories() {
    productCategoryRepository = mock(ProductCategoryRepository.class);
    for (ProductCategory productCategory : productCategories) {
      when(productCategoryRepository
              .findOne(productCategory.getId()))
              .thenReturn(productCategory);
    }
    for (ProductCategory productCategory : productCategories) {
      when(productCategoryRepository
              .save(productCategory))
              .thenReturn(productCategory);
    }
    for (ProductCategory productCategory : productCategories) {
      List<ProductCategory> matchedProductCategories = new ArrayList<>();
      for (ProductCategory productCategoryWithMatchedCode : productCategories) {
        if (productCategoryWithMatchedCode.getCode().equalsIgnoreCase(productCategory.getCode())) {
          matchedProductCategories.add(productCategoryWithMatchedCode);
        }
      }
      when(productCategoryRepository
              .searchProductCategories(productCategory.getCode()))
              .thenReturn(matchedProductCategories);
    }
    ReflectionTestUtils.setField(productCategoryService, "productCategoryRepository",
            productCategoryRepository, ProductCategoryRepository.class);
  }
}
