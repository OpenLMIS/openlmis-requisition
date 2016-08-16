package org.openlmis.referencedata.repository;

import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.springframework.beans.factory.annotation.Autowired;

public class ProductRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Product> {

  @Autowired
  ProductRepository repository;

  @Autowired
  ProductCategoryRepository productCategoryRepository;

  ProductRepository getRepository() {
    return this.repository;
  }

  Product generateInstance() {
    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

    int instanceNumber = this.getNextInstanceNumber();
    Product product = new Product();
    product.setCode("P" + instanceNumber);
    product.setPrimaryName("Product #" + instanceNumber);
    product.setDispensingUnit("unit");
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory1);
    return product;
  }
}
