package org.openlmis.product.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ProductCategoryServiceTest {

  @Mock
  private ProductCategoryRepository productCategoryRepository;

  @InjectMocks
  private ProductCategoryService productCategoryService;

  @Test
  public void shouldFindProductCategoryIfMatchedCode() {
    ProductCategory productCategory = mock(ProductCategory.class);
    String code = "test";

    when(productCategoryRepository
        .searchProductCategories(code))
        .thenReturn(Arrays.asList(productCategory));

    List<ProductCategory> receivedProductCategories =
        productCategoryService.searchProductCategories(code);

    assertEquals(1, receivedProductCategories.size());
    assertEquals(productCategory, receivedProductCategories.get(0));
  }
}
