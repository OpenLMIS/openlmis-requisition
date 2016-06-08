package org.openlmis.referencedata.repository;

import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Product;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class ProductRepositoryIT extends BaseCrudRepositoryIT<Product> {

    @Autowired
    ProductRepository repository;

    ProductRepository getRepository() {
        return this.repository;
    }

    Product generateInstance() {
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
        return product;
    }
}
