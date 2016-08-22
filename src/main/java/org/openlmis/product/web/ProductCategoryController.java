package org.openlmis.product.web;

import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.service.ProductCategoryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.UUID;

@RepositoryRestController
public class ProductCategoryController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductCategoryController.class);

  @Autowired
  private ProductCategoryService productCategoryService;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  /**
   * Allows creating new productCategories.
   *
   * @param productCategory A productCategory bound to the request body
   * @return ResponseEntity containing the created productCategory
   */
  @RequestMapping(value = "/productCategories", method = RequestMethod.POST)
  public ResponseEntity<?> createProductCategory(@RequestBody ProductCategory productCategory) {
    if (productCategory == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new productCategory");
      // Ignore provided id
      productCategory.setId(null);
      ProductCategory newProductCategory = productCategoryRepository.save(productCategory);
      return new ResponseEntity<ProductCategory>(newProductCategory, HttpStatus.CREATED);
    }
  }

  /**
   * Get all productCategories.
   *
   * @return ProductCategories.
   */
  @RequestMapping(value = "/productCategories", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllProductCategories() {
    Iterable<ProductCategory> productCategories = productCategoryRepository.findAll();
    if (productCategories == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(productCategories, HttpStatus.OK);
    }
  }

  /**
   * Allows updating productCategories.
   *
   * @param productCategory A productCategory bound to the request body
   * @param productCategoryId UUID of productCategory which we want to update
   * @return ResponseEntity containing the updated productCategory
   */
  @RequestMapping(value = "/productCategories/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProductCategory(@RequestBody ProductCategory productCategory,
                                       @PathVariable("id") UUID productCategoryId) {
    ProductCategory productCategoryFromDb = productCategoryRepository.findOne(productCategoryId);
    if (productCategoryFromDb == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Updating productCategory");
      ProductCategory updatedProductCategory = productCategoryRepository.save(productCategory);
      return new ResponseEntity<ProductCategory>(updatedProductCategory, HttpStatus.OK);
    }
  }

  /**
   * Get chosen productCategory.
   *
   * @param productCategoryId UUID of productCategory which we want to get
   * @return ProductCategory.
   */
  @RequestMapping(value = "/productCategories/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProductCategory(@PathVariable("id") UUID productCategoryId) {
    ProductCategory productCategory = productCategoryRepository.findOne(productCategoryId);
    if (productCategory == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(productCategory, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting productCategory.
   *
   * @param productCategoryId UUID of productCategory which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/productCategories/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProductCategory(@PathVariable("id") UUID productCategoryId) {
    ProductCategory productCategory = productCategoryRepository.findOne(productCategoryId);
    if (productCategory == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        productCategoryRepository.delete(productCategory);
      } catch (DataIntegrityViolationException ex) {
        LOGGER.debug("ProductCategory cannot be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<ProductCategory>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Finds ProductCategories matching all of provided parameters.
   * @param code code of productCategory.
   * @return ResponseEntity with list of all Product Categories matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/productCategories/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchProductCategories(
          @RequestParam(value = "code", required = false) String code) {
    List<ProductCategory> result = productCategoryService.searchProductCategories(code);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
