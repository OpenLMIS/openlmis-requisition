package org.openlmis.product.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.service.ProductCategoryService;
import org.openlmis.referencedata.web.BaseController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.UUID;

@Controller
public class ProductCategoryController extends BaseController {

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
    try {
      LOGGER.debug("Creating new productCategory");
      // Ignore provided id
      productCategory.setId(null);
      ProductCategory newProductCategory = productCategoryRepository.save(productCategory);
      LOGGER.debug("Creating new productCategory with id: " + productCategory.getId());
      return new ResponseEntity<ProductCategory>(newProductCategory, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating productCategory", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
    return new ResponseEntity<>(productCategories, HttpStatus.OK);
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
    try {
      LOGGER.debug("Updating productCategory with id: " + productCategoryId);
      ProductCategory updatedProductCategory = productCategoryRepository.save(productCategory);
      LOGGER.debug("Updated productCategory with id: " + productCategoryId);
      return new ResponseEntity<ProductCategory>(updatedProductCategory, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating productCategory", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
        ErrorResponse errorResponse =
              new ErrorResponse("ProductCategory cannot be deleted"
                    + "because of existing dependencies", ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
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
