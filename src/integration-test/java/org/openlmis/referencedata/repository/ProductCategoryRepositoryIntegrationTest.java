package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.repository.CrudRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class ProductCategoryRepositoryIntegrationTest extends
        BaseCrudRepositoryIntegrationTest<ProductCategory> {

  @Autowired
  ProductCategoryRepository repository;

  private List<ProductCategory> productCategories;

  CrudRepository<ProductCategory, UUID> getRepository() {
    return this.repository;
  }

  ProductCategory generateInstance() {
    ProductCategory productCategory = new ProductCategory();
    Integer instanceNumber = this.getNextInstanceNumber();
    productCategory.setName("productCategoryName" + instanceNumber);
    productCategory.setCode("productCategoryCode" + instanceNumber);
    productCategory.setDisplayOrder(instanceNumber);
    return productCategory;
  }

  @Before
  public void setUp() {
    productCategories = new ArrayList<>();
    for (int usersCount = 0; usersCount < 5; usersCount++) {
      productCategories.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchProductCategoriesByAllParameters() {
    List<ProductCategory> receivedProductCategories =
            repository.searchProductCategories(productCategories.get(0).getCode());

    Assert.assertEquals(1, receivedProductCategories.size());
    Assert.assertEquals(
            productCategories.get(0).getCode(),
            receivedProductCategories.get(0).getCode());
  }

  @Test
  public void testSearchProductCategoriesByAllParametersNull() {
    List<ProductCategory> receivedProductCategories =
            repository.searchProductCategories(null);

    Assert.assertEquals(productCategories.size(), receivedProductCategories.size());
  }
}
