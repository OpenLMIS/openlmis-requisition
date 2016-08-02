package org.openlmis.product.web;

import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.service.ProductCategoryService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RepositoryRestController
public class ProductCategoryController {

  @Autowired
  private ProductCategoryService productCategoryService;

  /**
   * Finds ProductCategories matching all of provided parameters.
   * @param code code of productCategory.
   * @return list of all ProductCategories matching all of provided parameters.
   */
  @RequestMapping(value = "/productCategories/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchProductCategories(
          @RequestParam(value = "code", required = false) String code) {
    List<ProductCategory> result = productCategoryService.searchProductCategories(code);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
