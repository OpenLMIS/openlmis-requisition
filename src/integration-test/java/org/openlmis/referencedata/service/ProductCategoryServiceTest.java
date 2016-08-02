package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.openlmis.Application;

import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.service.ProductCategoryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class ProductCategoryServiceTest {

  @Autowired
  ProductCategoryRepository productCategoryRepository;

  @Autowired
  ProductCategoryService productCategoryService;

  List<ProductCategory> productCategories;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    productCategories = new ArrayList<>();
    for ( int productCategoriesCount = 0; productCategoriesCount < 5; productCategoriesCount++ ) {
      productCategories.add(generateProductCategory());
    }
  }

  @After
  public void cleanup() {
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testSearchProductCategories() {
    List<ProductCategory> receivedProductCategories =
            productCategoryService.searchProductCategories(productCategories.get(0).getCode());

    Assert.assertEquals(1,receivedProductCategories.size());
    for ( ProductCategory productCategory : receivedProductCategories ) {
      Assert.assertEquals(productCategory.getCode(),productCategories.get(0).getCode());
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
}